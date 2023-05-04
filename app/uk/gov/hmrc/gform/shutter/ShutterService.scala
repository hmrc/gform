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

class ShutterService(
  shutterRepository: ShutterRepository,
  shutterFormTemplateRepository: ShutterFormTemplateRepository
)(implicit ec: ExecutionContext) {

  def findAllShutters(): Future[List[Shutter]] =
    shutterRepository.findAll()

  def findShutter(shutterMessageId: ShutterMessageId): Future[Option[Shutter]] =
    shutterRepository.find(shutterMessageId)

  def upsertShutter(shutter: Shutter): Future[Unit] =
    shutterRepository.upsert(shutter).map(_ => ())

  def deleteShutter(shutterMessageId: ShutterMessageId): Future[Unit] =
    shutterRepository.delete(shutterMessageId).map(_ => ())

  def upsertFormTemplateShutter(shutterFormTemplate: ShutterFormTemplate): Future[Unit] =
    shutterFormTemplateRepository.upsert(shutterFormTemplate).map(_ => ())

  def findFormTemplateShutter(formTemplateId: FormTemplateId): Future[Option[ShutterFormTemplate]] =
    shutterFormTemplateRepository.find(formTemplateId)

  def findByShutterMessageId(shutterMessageId: ShutterMessageId): Future[List[ShutterFormTemplate]] =
    shutterFormTemplateRepository.findByShutterMessageId(shutterMessageId)

  def deleteFormTemplateShutter(formTemplateId: FormTemplateId): Future[Unit] =
    shutterFormTemplateRepository.delete(formTemplateId).map(_ => ())

  def find(formTemplateId: FormTemplateId): Future[Option[Shutter]] =
    for {
      maybeShutterTemplate <- findFormTemplateShutter(formTemplateId)
      maybeShutter <-
        maybeShutterTemplate.fold(Future.successful(Option.empty[Shutter]))(template =>
          findShutter(template.shutterMessageId)
        )
    } yield maybeShutter

}
