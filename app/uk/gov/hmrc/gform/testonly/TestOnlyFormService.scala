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

import play.api.mvc.Results
import play.api.libs.json._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw
import uk.gov.hmrc.gform.save4later.FormMongoCache
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.BuildInfo
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import cats.implicits._

class TestOnlyFormService(
  snapshotMongoCache: SnapshotMongoCache,
  formMongoCache: FormMongoCache,
  jsonCrypto: Encrypter with Decrypter,
  formTemplateService: FormTemplateService,
  requestHandler: RequestHandlerAlg[FOpt]
)(implicit ec: ExecutionContext) {

  private def maybeReplaceDestinations(raw: FormTemplateRaw): FormTemplateRaw = {
    val destinations = (raw.value \ "destinations").as[JsArray]
    if (containTypes(destinations, List("hmrcIlluminate", "hmrcDms")))
      replaceDestination(raw)
    else
      raw
  }

  private def replaceDestination(raw: FormTemplateRaw): FormTemplateRaw = {
    val newDestinations = Json.arr(
      Json.obj(
        "id"            -> "transitionToSubmitted",
        "type"          -> "stateTransition",
        "requiredState" -> "Submitted"
      )
    )
    val updatedValue = raw.value ++ Json.obj("destinations" -> newDestinations)
    FormTemplateRaw(updatedValue)
  }

  private def containTypes(destinations: JsArray, types: List[String]): Boolean =
    destinations.value.exists { jsValue =>
      val destinationType = (jsValue \ "type").as[String]
      types.contains(destinationType)
    }

  private def updateShowContinueOrDeletePage(raw: FormTemplateRaw): FormTemplateRaw = {
    val updatedValue = raw.value ++ Json.obj("showContinueOrDeletePage" -> "false")
    FormTemplateRaw(updatedValue)
  }

  def saveForm(saveRequest: SaveRequest)(implicit hc: HeaderCarrier): Future[SnapshotOverview] =
    formMongoCache
      .get(saveRequest.formId)
      .flatMap { form =>
        for {
          raw <- formTemplateService.get(FormTemplateRawId(form.formTemplateId.value))
          rawUpdated = updateShowContinueOrDeletePage(maybeReplaceDestinations(raw))
          snapshot = Snapshot(
                       form,
                       rawUpdated,
                       saveRequest.description,
                       GformVersion(BuildInfo.version),
                       saveRequest.gformFrontendVersion,
                       saveRequest.ggFormData
                     )
          _ <- snapshotMongoCache.put(snapshot.snapshotId, snapshot)
          _ <- restoreSnapshotTemplate(snapshot.snapshotId)
        } yield SnapshotOverview(snapshot, withData = true)
      }

  def restoreSnapshotTemplate(snapshotId: SnapshotId): Future[Unit] =
    snapshotMongoCache.find(snapshotId).flatMap {
      case Some(snapshot) =>
        requestHandler
          .handleRequest(snapshot.toSnapshotTemplate())
          .fold(_ => throw new Exception(s"Unable to create a new template"), _ => Results.Ok)
          .map(_ => ())
      case None => throw new Exception(s"We could not find snapshot item with id: $snapshotId")
    }

  def restoreForm(snapshotId: SnapshotId, restoreId: String, useOriginalTemplate: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[SnapshotOverview] = {
    val snapshotF = snapshotMongoCache.find(snapshotId)
    val currentFormF = formMongoCache.find(FormId(restoreId))
    (snapshotF, currentFormF)
      .mapN {
        case (Some(snapshot), Some(currentForm)) => (snapshot, currentForm)
        case _                                   => throw new Exception(s"We could not find cache item with id: $snapshotId or $restoreId")
      }
      .flatMap { case (snapshot, currentForm) =>
        formMongoCache
          .upsert(snapshot.toSnapshotForm(currentForm, useOriginalTemplate))
          .map(_ => SnapshotOverview(snapshot, withData = true))
      }
  }

  def getSnapshots(filter: SnapshotFilter): Future[List[SnapshotOverview]] =
    snapshotMongoCache.findWithFilter(filter).map(_.map(SnapshotOverview(_, withData = false)))

  def getSnapshotData(snapshotId: SnapshotId): Future[SnapshotOverview] =
    snapshotMongoCache.find(snapshotId).map {
      case Some(snapshot) => SnapshotOverview(snapshot, withData = true)
      case None           => throw new Exception(s"We could not find snapshot item with id: $snapshotId")
    }

  def updateSnapshot(request: UpdateSnapshotRequest): Future[SnapshotOverview] =
    snapshotMongoCache
      .find(request.snapshotId)
      .map {
        case Some(snapshot) =>
          val newDescription = request.description
          snapshot.updateWith(request.formData, newDescription)
        case None => throw new Exception(s"We could not find snapshot item with id: $request.snapshotId")
      }
      .flatMap { updatedSnapshot =>
        snapshotMongoCache.upsert(updatedSnapshot).map(_ => SnapshotOverview(updatedSnapshot, withData = true))
      }

  def updateFormData(request: UpdateFormDataRequest)(implicit hc: HeaderCarrier): Future[SaveReply] =
    formMongoCache
      .find(request.formId)
      .map {
        case Some(form) =>
          form.copy(formData = request.formData)
        case None => throw new Exception(s"We could not find snapshot item with id: $request.snapshotId")
      }
      .flatMap { updatedForm =>
        formMongoCache.upsert(updatedForm).map(_ => SaveReply(request.formId))
      }

  def deleteSnapshot(snapshotId: SnapshotId): Future[Unit] =
    snapshotMongoCache.delete(snapshotId)

}
