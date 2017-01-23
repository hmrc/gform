/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.controllers
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.json._
import play.api.mvc.{ Action, Result }
import uk.gov.hmrc.bforms.model.{ INVALID_DATA, Other, RESPONSE_OK }
import uk.gov.hmrc.bforms.repositories.SaveAndRetrieveRepository
import uk.gov.hmrc.bforms.services.{ Retrieve, RetrieveService, Save, SaveService }
import uk.gov.hmrc.play.microservice.controller.BaseController
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
class SaveAndRetrieveController(val messagesApi: MessagesApi)(implicit repository: SaveAndRetrieveRepository) extends SaveAndRetrieveHelperController {
  implicit val save: Save[JsValue, String] = Save.saveData(repository)
  implicit val retrieve: Retrieve[String, JsValue] = Retrieve.retrieveFormData(repository)
  def saveForm(registrationNumber: String) = saveFormData(registrationNumber)
  def retrieveForm(registrationNumber: String) = retrieveFormData(registrationNumber)
}
trait SaveAndRetrieveHelperController extends BaseController with I18nSupport {
  implicit val save: Save[JsValue, String]
  implicit val retrieve: Retrieve[String, JsValue]
  def saveFormData(registrationNumber: String)(implicit repo: SaveAndRetrieveRepository): Action[JsValue] = Action.async(parse.json) { implicit request =>
    val messages = messagesApi.preferred(request)
    implicit val reads: Reads[JsValue] = JsPath.read(request.body)
    request.body.validate(reads) match {
      case JsSuccess(req, _) =>
        SaveService.save(request.body, registrationNumber)(save).map {
          case Left(x) => Ok(INVALID_DATA.toJson(messages))
          case Right(()) => Ok(RESPONSE_OK.toJson(messages))
        }
      case JsError(jsonErrors) =>
        Future.successful(BadRequest(Json.obj("message" -> JsError.toJson(jsonErrors))))
    }
  }
  def retrieveFormData(registrationNumber: String) = Action.async { implicit request =>
    RetrieveService.retrieve(registrationNumber)(retrieve).map {
      case Left(x) => Ok(x)
      case Right(()) => Ok(Json.obj())
    }
  }
}
