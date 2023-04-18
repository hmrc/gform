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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class NotificationBannerFormTemplateService(
  repo: NotificationBannerFormTemplateRepository
)(implicit ec: ExecutionContext) {

  def upsert(notificationBannerFormTemplate: NotificationBannerFormTemplate): Future[Unit] =
    repo.upsert(notificationBannerFormTemplate).map(_ => ())

  def find(formTemplateId: FormTemplateId): Future[Option[NotificationBannerFormTemplate]] =
    repo.find(formTemplateId)

  def findByBannerId(bannerId: BannerId): Future[List[NotificationBannerFormTemplate]] =
    repo.findByBannerId(bannerId)

  def delete(formTemplateId: FormTemplateId): Future[Unit] =
    repo.delete(formTemplateId).map(_ => ())
}
