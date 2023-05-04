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

import cats.implicits._
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.BannerId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.ExecutionContext

class NotificationBannerController(
  notificationBannerService: NotificationBannerService,
  notificationBannerFormTemplateService: NotificationBannerFormTemplateService,
  notificationService: NotificationService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def findAllNotificationBanners(): Action[AnyContent] =
    Action.async { _ =>
      notificationBannerService
        .findAll()
        .flatMap(notificationBanners =>
          notificationBanners.traverse(notificationBanner =>
            for {
              res <- notificationBannerFormTemplateService.findByBannerId(notificationBanner._id)
            } yield NotificationBannerView(
              notificationBanner._id,
              notificationBanner.message,
              notificationBanner.isGlobal,
              res.map(_._id)
            )
          )
        )
        .map(r => Ok(Json.toJson(r)))
    }

  def find(formTemplateId: FormTemplateId): Action[AnyContent] =
    Action.async { request =>
      notificationService
        .find(formTemplateId)
        .map(_.fold(NoContent)(notificationBanner => Ok(Json.toJson(notificationBanner))))
    }

  def upsertNotificationBanner(): Action[NotificationBanner] =
    Action.async(parse.json[NotificationBanner]) { request =>
      val notificationBanner = request.body
      notificationBannerService.upsert(notificationBanner).map { _ =>
        NoContent
      }
    }

  def deleteNotificationBanner(bannerId: BannerId): Action[AnyContent] =
    Action.async { request =>
      for {
        _ <- notificationBannerFormTemplateService.findByBannerId(bannerId).map { bannerFormTemplates =>
               bannerFormTemplates.traverse { bannerFormTemplate =>
                 notificationBannerFormTemplateService.delete(bannerFormTemplate._id)
               }
             }
        _ <- notificationBannerService.delete(bannerId)
      } yield NoContent
    }
}
