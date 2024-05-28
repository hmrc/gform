/*
 * Copyright 2023 HM Revenue & Customs
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

import org.apache.pekko.stream.Materializer
import play.api.mvc.{ ControllerComponents, Results }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.{ ObjectStoreAlgebra, ObjectStorePaths }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class TestOnlyObjectStoreController(
  controllerComponents: ControllerComponents,
  objectStoreAlgebra: ObjectStoreAlgebra[Future]
)(implicit
  ex: ExecutionContext,
  m: Materializer
) extends BaseController(controllerComponents) {

  def downloadDmsFiles(envelopeId: EnvelopeId) = Action.async { implicit request =>
    val paths = SdesDestination.Dms.objectStorePaths(envelopeId)
    val fileName = s"${envelopeId.value}.zip"
    objectStoreAlgebra
      .getZipFile(envelopeId, paths)
      .map {
        case Some(objectSource) =>
          Ok.streamed(
            objectSource.content,
            contentLength = Some(objectSource.metadata.contentLength),
            contentType = Some(objectSource.metadata.contentType)
          ).as(ContentType.`application/zip`.value)
            .withHeaders(
              Results.contentDispositionHeader(inline = false, name = Some(fileName)).toList: _*
            )
        case None => BadRequest(s"File ${paths.ephemeral.value}/$fileName not found")
      }
  }

  def downloadDataStoreFile(envelopeId: EnvelopeId) = Action.async { implicit request =>
    val paths = SdesDestination.DataStore.objectStorePaths(envelopeId)
    downloadJsonFile(paths, envelopeId)
  }

  def downloadHmrcIlluminateFile(envelopeId: EnvelopeId) = Action.async { implicit request =>
    val paths = SdesDestination.HmrcIlluminate.objectStorePaths(envelopeId)
    downloadJsonFile(paths, envelopeId)
  }

  private def downloadJsonFile(paths: ObjectStorePaths, envelopeId: EnvelopeId)(implicit
    hc: HeaderCarrier
  ) = {
    val fileName = s"${envelopeId.value}.json"
    objectStoreAlgebra
      .getFile(paths.permanent, fileName)
      .map {
        case Some(objectSource) =>
          Ok.streamed(
            objectSource.content,
            contentLength = Some(objectSource.metadata.contentLength),
            contentType = Some(objectSource.metadata.contentType)
          ).as(ContentType.`application/json`.value)
            .withHeaders(
              Results.contentDispositionHeader(inline = false, name = Some(fileName)).toList: _*
            )
        case None => BadRequest(s"File ${paths.permanent.value}/$fileName not found")
      }
  }

}
