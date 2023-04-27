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

import uk.gov.hmrc.gform.sharedmodel.ShutterMessageId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import scala.concurrent.{ ExecutionContext, Future }

class ShutterFormTemplateService(
  repo: ShutterFormTemplateRepository
)(implicit ec: ExecutionContext) {

  def upsert(shutterFormTemplate: ShutterFormTemplate): Future[Unit] =
    repo.upsert(shutterFormTemplate).map(_ => ())

  def find(formTemplateId: FormTemplateId): Future[Option[ShutterFormTemplate]] =
    repo.find(formTemplateId)

  def findByShutterMessageId(shutterMessageId: ShutterMessageId): Future[List[ShutterFormTemplate]] =
    repo.findByShutterMessageId(shutterMessageId)

  def delete(formTemplateId: FormTemplateId): Future[Unit] =
    repo.delete(formTemplateId).map(_ => ())
}
