/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.testonly

import play.api.libs.json._
import uk.gov.hmrc.mongo.cache.DataKey
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.form.FormData

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import cats.implicits._

class TestOnlyFormService(
  formRepository: MongoCacheRepository[String],
  snapshotRepository: MongoCacheRepository[String],
  jsonCrypto: Encrypter with Decrypter
)(implicit ec: ExecutionContext) {

  def saveForm(saveRequest: SaveRequest): Future[SaveReply] =
    formRepository.findById(saveRequest.formId).flatMap {
      case Some(cacheItem) =>
        val newId = java.util.UUID.randomUUID().toString
        cacheItem.data.fields.toList
          .traverse { case (k, v) =>
            val value = if (k == "form") { v.as[JsObject] - "_id" - "userId" }
            else v
            snapshotRepository.put(newId)(DataKey(k), value)
          }
          .flatMap(_ => snapshotRepository.put(newId)(DataKey("description"), saveRequest.description))
          .map(_ => SaveReply(newId))
      case None =>
        Future.failed(new Exception(s"We could not find cache item with id: ${saveRequest.formId}"))
    }

  def restoreForm(snapshotId: String, restoreId: String): Future[Snapshot] = {
    val snapshotItem = snapshotRepository.findById(snapshotId)
    val restoreItem = formRepository.findById(restoreId)
    (snapshotItem, restoreItem)
      .mapN {
        case (Some(snapshotCacheItem), Some(restoreCacheItem)) => (snapshotCacheItem, restoreCacheItem)
        case _                                                 => throw new Exception(s"We could not find cache item with id: $snapshotId or $restoreId")
      }
      .flatMap { case (snapshotCacheItem, restoreCacheItem) =>
        val updatedData = updateCacheItemData(snapshotCacheItem.data, restoreCacheItem.data)
        updatedData.fields.toList
          .traverse { case (k, v) =>
            formRepository.put(restoreId)(DataKey(k), v)
          }
          .map(_ => Snapshot(snapshotCacheItem))
      }

  }

  private def updateCacheItemData(snapshotData: JsObject, formData: JsObject): JsObject = {
    val id = formData.validate((__ \ "form" \ "_id").json.pick).get
    val userId = formData.validate((__ \ "form" \ "userId").json.pick).get
    snapshotData
      .transform(
        (__ \ "form").json.update(
          __.read[JsObject].map { o =>
            o ++ Json.obj("_id" -> id) ++ Json.obj("userId" -> userId)
          }
        )
      )
      .map(_ - "description")
      .getOrElse(snapshotData)
  }

  def getSnapshots(): Future[List[Snapshot]] =
    snapshotRepository.collection
      .find()
      .toFuture()
      .map { list =>
        list.toList.map(Snapshot(_))
      }

  def getSnapshotData(snapshotId: String): Future[SnapshotWithData] = {
    val snapshotItem = snapshotRepository.findById(snapshotId)
    snapshotItem.map {
      case Some(snapshotCacheItem) =>
        val snapshot = Snapshot(snapshotCacheItem)
        val data = snapshotCacheItem.data.as[JsObject].value("form").as[JsObject].value("formData").as[String]
        SnapshotWithData(
          snapshot,
          Json.toJson(Json.parse(jsonCrypto.decrypt(Crypted(data)).value).as[FormData]).as[JsObject]
        )
      case None => throw new Exception(s"We could not find cache item with id: $snapshotId")
    }
  }

  def updateSnapshot(request: UpdateSnapshotRequest): Future[SaveReply] =
    snapshotRepository.findById(request.snapshotId).flatMap {
      case Some(cacheItem) =>
        val updatedFormData =
          Json.obj("formData" -> jsonCrypto.encrypt(PlainText(Json.toJson(request.formData).toString())).value)
        val data = cacheItem.data.as[JsObject].validate((__ \ "form").json.pick).get
        val updatedForm = data
          .transform(
            __.json.update(
              __.read[JsObject].map { o =>
                o ++ updatedFormData
              }
            )
          )
          .getOrElse(data)
        snapshotRepository
          .put(request.snapshotId)(DataKey("form"), updatedForm)
          .flatMap(_ => snapshotRepository.put(request.snapshotId)(DataKey("description"), request.description))
          .map(_ => SaveReply(request.snapshotId))
      case None =>
        Future.failed(new Exception(s"We could not find cache item with id: ${request.snapshotId}"))
    }

}
