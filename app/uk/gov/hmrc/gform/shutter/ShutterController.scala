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
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def findAllShutters(): Action[AnyContent] =
    Action.async { _ =>
      shutterService
        .findAllShutters()
        .flatMap(shutters =>
          shutters.traverse(shutter =>
            for {
              res <- shutterService.findByShutterMessageId(shutter._id)
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
        maybeShutterTemplate <- shutterService.findFormTemplateShutter(formTemplateId)
        maybeShutter <-
          maybeShutterTemplate.fold(Future.successful(Option.empty[Shutter]))(template =>
            shutterService.findShutter(template.shutterMessageId)
          )
      } yield maybeShutter.fold(NoContent)(shutter => Ok(Json.toJson(shutter)))
    }

  def upsertShutter(): Action[Shutter] =
    Action.async(parse.json[Shutter]) { request =>
      val shutter = request.body
      shutterService.upsertShutter(shutter).map { _ =>
        NoContent
      }
    }

  def deleteShutter(shutterMessageId: ShutterMessageId): Action[AnyContent] =
    Action.async { request =>
      for {
        _ <- shutterService.findByShutterMessageId(shutterMessageId).map { shutterMessageFormTemplates =>
               shutterMessageFormTemplates.traverse { shutterMessageFormTemplate =>
                 shutterService.deleteFormTemplateShutter(shutterMessageFormTemplate._id)
               }
             }
        _ <- shutterService.deleteShutter(shutterMessageId)
      } yield NoContent
    }

  def upsertFormTemplateShutter(): Action[ShutterFormTemplate] =
    Action.async(parse.json[ShutterFormTemplate]) { request =>
      val shutterFormTemplate = request.body
      shutterService.upsertFormTemplateShutter(shutterFormTemplate).map { _ =>
        NoContent
      }
    }

  def deleteFormTemplateShutter(formTemplateId: FormTemplateId): Action[AnyContent] =
    Action.async { request =>
      shutterService.deleteFormTemplateShutter(formTemplateId).map { _ =>
        NoContent
      }
    }
}
