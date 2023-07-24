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

import akka.stream.Materializer
import play.api.mvc.{ ControllerComponents, Results }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.objectstore.client.Path

import scala.concurrent.{ ExecutionContext, Future }

class TestOnlyObjectStoreController(
  controllerComponents: ControllerComponents,
  objectStoreAlgebra: ObjectStoreAlgebra[Future],
  dataStoreBasePath: String
)(implicit
  ex: ExecutionContext,
  m: Materializer
) extends BaseController(controllerComponents) {

  def downloadDmsFiles(envelopeId: EnvelopeId) = Action.async { implicit request =>
    objectStoreAlgebra.getZipFile(envelopeId) map {
      case Some(objectSource) =>
        Ok.streamed(
          objectSource.content,
          contentLength = Some(objectSource.metadata.contentLength),
          contentType = Some(objectSource.metadata.contentType)
        ).as("application/zip")
          .withHeaders(
            Results.contentDispositionHeader(inline = false, name = Some(s"${envelopeId.value}.zip")).toList: _*
          )
      case None => BadRequest(s"Envelope with id: $envelopeId not found")
    }
  }

  def downloadDataStoreFile(envelopeId: EnvelopeId) = Action.async { implicit request =>
    objectStoreAlgebra.getFile(
      Path.Directory(s"${dataStoreBasePath}envelopes/${envelopeId.value}"),
      s"${envelopeId.value}.json"
    ) map {
      case Some(objectSource) =>
        Ok.streamed(
          objectSource.content,
          contentLength = Some(objectSource.metadata.contentLength),
          contentType = Some(objectSource.metadata.contentType)
        ).as(ContentType.`application/json`.value)
          .withHeaders(
            Results.contentDispositionHeader(inline = false, name = Some(s"${envelopeId.value}.json")).toList: _*
          )
      case None => BadRequest(s"Envelope with id: $envelopeId not found")
    }
  }

}
