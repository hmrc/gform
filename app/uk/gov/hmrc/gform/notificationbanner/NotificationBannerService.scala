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

import uk.gov.hmrc.gform.sharedmodel.BannerId

import scala.concurrent.{ ExecutionContext, Future }

class NotificationBannerService(
  notificationBannerRepository: NotificationBannerRepository
)(implicit ec: ExecutionContext) {

  def findAll(): Future[List[NotificationBanner]] =
    notificationBannerRepository.findAll()

  def find(bannerId: BannerId): Future[Option[NotificationBanner]] =
    notificationBannerRepository.find(bannerId)

  def findGlobal(): Future[Option[NotificationBanner]] =
    notificationBannerRepository.findGlobal()

  def upsert(notificationBanner: NotificationBanner): Future[Unit] =
    notificationBannerRepository.upsert(notificationBanner).map(_ => ())

  def delete(bannerId: BannerId): Future[Unit] =
    notificationBannerRepository.delete(bannerId).map(_ => ())

}
