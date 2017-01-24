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
import uk.gov.hmrc.bforms.model._
import uk.gov.hmrc.bforms.repositories.SaveAndRetrieveRepository
import uk.gov.hmrc.bforms.services.{ RetrieveService, SaveService }
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global

class SaveAndRetrieveController(val messagesApi: MessagesApi)(implicit repository: SaveAndRetrieveRepository) extends SaveAndRetrieveHelperController {

  def saveForm(registrationNumber: String) = saveFormData(registrationNumber)

  def retrieveForm(registrationNumber: String) = retrieveFormData(registrationNumber)

}

trait SaveAndRetrieveHelperController extends BaseController with I18nSupport {

  def saveFormData(registrationNumber: String)(implicit repo: SaveAndRetrieveRepository): Action[SaveAndRetrieve] = Action.async(parse.json[SaveAndRetrieve]) { implicit request =>
    SaveService.save(request.body, registrationNumber).map {
      case Left(err) => err.toResult
      case Right(dbSuccess) => dbSuccess.toResult
    }
  }

  def retrieveFormData(registrationNumber: String)(implicit repo: SaveAndRetrieveRepository) = Action.async { implicit request =>
    RetrieveService.retrieve(registrationNumber).map {
      case Some(x) => Ok(x.value)
      case None => Ok(Json.obj())
    }
  }
}