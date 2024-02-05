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
import play.api.mvc.Results
import uk.gov.hmrc.mongo.cache.DataKey
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }

import uk.gov.hmrc.gform.core.FOpt

import uk.gov.hmrc.gform.BuildInfo

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import cats.implicits._
import java.time.Instant

class TestOnlyFormService(
  formRepository: MongoCacheRepository[String],
  snapshotRepository: MongoCacheRepository[String],
  jsonCrypto: Encrypter with Decrypter,
  formTemplateService: FormTemplateService,
  requestHandler: RequestHandlerAlg[FOpt]
)(implicit ec: ExecutionContext) {

  def saveForm(saveRequest: SaveRequest): Future[SnapshotWithData] =
    formRepository
      .findById(saveRequest.formId)
      .flatMap {
        case Some(cacheItem) =>
          val newId = java.util.UUID.randomUUID().toString
          cacheItem.data.fields.toList
            .traverse { case (k, v) =>
              if (k === "form") {
                val formTemplateId = v.as[JsObject].value("formTemplateId").as[String]
                val prefix = UniqueStringGenerator.generateUniqueString()
                val updatedFormTemplateId = s"${prefix}_$formTemplateId"
                val js = v.as[JsObject] - "_id" - "userId" ++ Json.obj("formTemplateId" -> updatedFormTemplateId)
                for {
                  _   <- snapshotRepository.put(newId)(DataKey(k), js)
                  raw <- formTemplateService.get(FormTemplateRawId(formTemplateId))
                  updatedRaw = raw.value ++ Json.obj("_id" -> updatedFormTemplateId)
                  _ <- snapshotRepository.put(newId)(DataKey("template"), updatedRaw)
                  _ <- restoreSnapshotTemplate(newId)
                } yield ()
              } else {
                snapshotRepository.put(newId)(DataKey(k), v)
              }
            }
            .flatMap(_ =>
              snapshotRepository.put(newId)(
                DataKey("description"),
                saveRequest.description
              )
            )
            .flatMap(_ =>
              snapshotRepository.put(newId)(
                DataKey("gformVersion"),
                Json.toJson(BuildInfo.version)
              )
            )
            .flatMap(_ =>
              snapshotRepository.put(newId)(
                DataKey("gformFrontendVersion"),
                saveRequest.gformFrontendVersion
              )
            )
            .map(_ => newId)
        case None =>
          Future.failed(new Exception(s"We could not find cache item with id: ${saveRequest.formId}"))
      }
      .flatMap(snapShotId => getSnapshotData(snapShotId))

  def restoreSnapshotTemplate(snapshotId: String): Future[Unit] = {
    val snapshotItem = snapshotRepository.findById(snapshotId)
    snapshotItem.flatMap {
      case Some(snapshotCacheItem) =>
        val templateJsonOption: Option[JsObject] =
          snapshotCacheItem.data.validate((__ \ "template").read[JsObject]).asOpt
        templateJsonOption
          .map { templateJson =>
            requestHandler
              .handleRequest(FormTemplateRaw(templateJson))
              .fold(_.asBadRequest, _ => Results.Ok)
              .map(_ => ())
          }
          .getOrElse(().pure[Future])
      case None => throw new Exception(s"We could not restore the template for : $snapshotId")
    }
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
      .map(_ - "template")
      .map(_ - "gformVersion")
      .map(_ - "gformFrontendVersion")
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

  def updateSnapshot(request: UpdateSnapshotRequest): Future[SnapshotWithData] =
    snapshotRepository
      .findById(request.snapshotId)
      .flatMap {
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
            .map(_ => request.snapshotId)
        case None =>
          Future.failed(new Exception(s"We could not find cache item with id: ${request.snapshotId}"))
      }
      .flatMap(snapShotId => getSnapshotData(snapShotId))

  def updateFormData(request: UpdateFormDataRequest): Future[SaveReply] =
    formRepository.findById(request.formId).flatMap {
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
        formRepository
          .put(request.formId)(DataKey("form"), updatedForm)
          .map(_ => SaveReply(request.formId))
      case None =>
        Future.failed(new Exception(s"We could not find cache item with id: ${request.formId}"))
    }

}

object UniqueStringGenerator {
  val base = 32
  // removing 4 ambiguous characters (1, l, 0, o)
  val alphabet = ('2' to '9') ++ ('a' to 'k') ++ ('m' to 'n') ++ ('p' to 'z')

  def encode(number: Long): String = {
    def encodeRecursive(num: Long, acc: String): String =
      if (num == 0) {
        if (acc.isEmpty) "a" else acc
      } else {
        val remainder = (num % base).toInt
        val nextChar = alphabet(remainder)
        encodeRecursive(num / base, s"$nextChar$acc")
      }
    encodeRecursive(number, "")
  }
  def generateUniqueString(): String = {
    val currentTimestamp = Instant.now.toEpochMilli

    // Subtract a fixed timestamp to reduce the number's size
    val fixedTimestamp = Instant.parse("2024-01-01T00:00:00Z").toEpochMilli

    val reducedTimestamp = (currentTimestamp - fixedTimestamp) / 1000
    encode(reducedTimestamp)
  }
}
