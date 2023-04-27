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

package uk.gov.hmrc.gform.shutter

import cats.implicits._
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.ShutterMessageId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class ShutterController(
  shutterService: ShutterService,
  shutterFormTemplateService: ShutterFormTemplateService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def findAllShutters(): Action[AnyContent] =
    Action.async { _ =>
      shutterService
        .findAll()
        .flatMap(shutters =>
          shutters.traverse(shutter =>
            for {
              res <- shutterFormTemplateService.findByShutterMessageId(shutter._id)
            } yield ShutterView(
              shutter._id,
              shutter.message,
              res.map(_._id)
            )
          )
        )
        .map(r => Ok(Json.toJson(r)))
    }
  def find(formTemplateId: FormTemplateId): Action[AnyContent] =
    Action.async { request =>
      for {
        maybeShutterTemplate <- shutterFormTemplateService.find(formTemplateId)
        maybeShutter <-
          maybeShutterTemplate.fold(Future.successful(Option.empty[Shutter]))(template =>
            shutterService.find(template.shutterMessageId)
          )
      } yield maybeShutter.fold(NoContent)(shutter => Ok(Json.toJson(shutter)))
    }

  def upsertShutter(): Action[Shutter] =
    Action.async(parse.json[Shutter]) { request =>
      val shutter = request.body
      shutterService.upsert(shutter).map { _ =>
        NoContent
      }
    }

  def deleteShutter(shutterMessageId: ShutterMessageId): Action[AnyContent] =
    Action.async { request =>
      for {
        _ <- shutterFormTemplateService.findByShutterMessageId(shutterMessageId).map { shutterMessageFormTemplates =>
               shutterMessageFormTemplates.traverse { shutterMessageFormTemplate =>
                 shutterFormTemplateService.delete(shutterMessageFormTemplate._id)
               }
             }
        _ <- shutterService.delete(shutterMessageId)
      } yield NoContent
    }
}
