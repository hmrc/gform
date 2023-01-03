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

import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.controllers.BaseController

class NotificationBannerController(
  notificationBannerService: NotificationBannerService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def findNotificationBanner(): Action[AnyContent] =
    Action.async { request =>
      notificationBannerService.find().map { maybeNotificationBanner =>
        maybeNotificationBanner.fold(NoContent) { notificationBanner =>
          Ok(Json.toJson(notificationBanner))
        }
      }
    }

  def upsertNotificationBanner(): Action[NotificationBanner] =
    Action.async(parse.json[NotificationBanner]) { request =>
      val notificationBanner = request.body
      notificationBannerService.upsert(notificationBanner).map { _ =>
        NoContent
      }
    }

  def deleteNotificationBanner(): Action[AnyContent] =
    Action.async { request =>
      notificationBannerService.delete().map { _ =>
        NoContent
      }
    }
}
