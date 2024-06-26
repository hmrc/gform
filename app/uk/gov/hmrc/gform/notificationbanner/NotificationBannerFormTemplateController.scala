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

package uk.gov.hmrc.gform.notificationbanner

import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.ExecutionContext

class NotificationBannerFormTemplateController(
  service: NotificationBannerFormTemplateService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def upsert(): Action[NotificationBannerFormTemplate] =
    Action.async(parse.json[NotificationBannerFormTemplate]) { request =>
      val notificationBannerFormTemplate = request.body
      service.upsert(notificationBannerFormTemplate).map { _ =>
        NoContent
      }
    }

  def delete(formTemplateId: FormTemplateId): Action[AnyContent] =
    Action.async { request =>
      service.delete(formTemplateId).map { _ =>
        NoContent
      }
    }

}
