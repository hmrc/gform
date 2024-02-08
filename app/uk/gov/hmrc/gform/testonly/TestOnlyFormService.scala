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
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRawId
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, RequestHandlerAlg }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
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

  def saveForm(saveRequest: SaveRequest)(implicit hc: HeaderCarrier): Future[SnapshotOverview] =
    formMongoCache
      .get(saveRequest.formId)
      .flatMap { form =>
        for {
          raw <- formTemplateService.get(FormTemplateRawId(form.formTemplateId.value))
          snapshotItem = SnapshotItem(
                           form,
                           raw,
                           saveRequest.description,
                           GformVersion(BuildInfo.version),
                           saveRequest.gformFrontendVersion
                         )
          _ <- snapshotMongoCache.put(snapshotItem.snapshotId, snapshotItem)
          _ <- restoreSnapshotTemplate(snapshotItem.snapshotId)
        } yield SnapshotOverview(snapshotItem, withData = true)
      }

  def restoreSnapshotTemplate(snapshotId: SnapshotId): Future[Unit] =
    snapshotMongoCache.find(snapshotId).flatMap {
      case Some(snapshotItem) =>
        requestHandler
          .handleRequest(snapshotItem.toSnapshotTemplate())
          .fold(_ => throw new Exception(s"Unable to create a new template"), _ => Results.Ok)
          .map(_ => ())
      case None => throw new Exception(s"We could not find snapshot item with id: $snapshotId")
    }

  def restoreForm(snapshotId: SnapshotId, restoreId: String)(implicit hc: HeaderCarrier): Future[SnapshotOverview] = {
    val snapshotItemF = snapshotMongoCache.find(snapshotId)
    val currentFormF = formMongoCache.find(FormId(restoreId))
    (snapshotItemF, currentFormF)
      .mapN {
        case (Some(snapshotItem), Some(currentForm)) => (snapshotItem, currentForm)
        case _                                       => throw new Exception(s"We could not find cache item with id: $snapshotId or $restoreId")
      }
      .flatMap { case (snapshotItem, currentForm) =>
        formMongoCache
          .upsert(snapshotItem.toSnapshotForm(currentForm))
          .map(_ => SnapshotOverview(snapshotItem, withData = true))
      }
  }

  def getSnapshots(): Future[List[SnapshotOverview]] =
    snapshotMongoCache.findAll().map(_.map(SnapshotOverview(_, withData = false)))

  def getSnapshotData(snapshotId: SnapshotId): Future[SnapshotOverview] =
    snapshotMongoCache.find(snapshotId).map {
      case Some(snapshotItem) => SnapshotOverview(snapshotItem, withData = true)
      case None               => throw new Exception(s"We could not find snapshot item with id: $snapshotId")
    }

  def updateSnapshot(request: UpdateSnapshotRequest): Future[SnapshotOverview] =
    snapshotMongoCache
      .find(request.snapshotId)
      .map {
        case Some(snapshotItem) =>
          val newFormData = request.formData.validate[FormData].get
          val newDescription = request.description
          snapshotItem.updateWith(newFormData, newDescription)
        case None => throw new Exception(s"We could not find snapshot item with id: $request.snapshotId")
      }
      .flatMap { updatedSnapshotItem =>
        snapshotMongoCache.upsert(updatedSnapshotItem).map(_ => SnapshotOverview(updatedSnapshotItem, withData = true))
      }

  def updateFormData(request: UpdateFormDataRequest)(implicit hc: HeaderCarrier): Future[SaveReply] =
    formMongoCache
      .find(request.formId)
      .map {
        case Some(form) =>
          val newFormData = request.formData.validate[FormData].get
          form.copy(formData = newFormData)
        case None => throw new Exception(s"We could not find snapshot item with id: $request.snapshotId")
      }
      .flatMap { updatedForm =>
        formMongoCache.upsert(updatedForm).map(_ => SaveReply(request.formId))
      }

}
