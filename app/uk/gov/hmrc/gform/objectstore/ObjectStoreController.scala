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

package uk.gov.hmrc.gform.objectstore

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }

import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreController(controllerComponents: ControllerComponents, objectStoreAlgebra: ObjectStoreAlgebra[Future])(
  implicit ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId) = Action.async { implicit request =>
    objectStoreAlgebra.deleteFile(envelopeId, fileId).asNoContent
  }

  def deleteFiles(envelopeId: EnvelopeId) = Action.async(parse.json[Set[FileId]]) { implicit request =>
    objectStoreAlgebra.deleteFiles(envelopeId, request.body).asNoContent
  }
}
