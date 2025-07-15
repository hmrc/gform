/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.emailmigration

import play.api.mvc.{ Action, ControllerComponents }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formmetadata.FormMetadataAlgebra
import uk.gov.hmrc.gform.save4later.FormPersistenceAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, Submitted }
import uk.gov.hmrc.gform.sharedmodel.{ EmailToGGMigration, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.FormIdData
import uk.gov.hmrc.http.HeaderCarrier

class EmailMigrationController(
  formPersistence: FormPersistenceAlgebra[Future],
  formMetadataService: FormMetadataAlgebra[Future],
  controllerComponents: ControllerComponents
)(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def reconcileForms(
    maybeEmailForm: Option[Form],
    maybeGGForm: Option[Form],
    emailFormIdData: FormIdData,
    formIdData: FormIdData
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    (maybeEmailForm, maybeGGForm) match {
      case (Some(emailForm), Some(ggForm)) =>
        Future.failed(new Exception(s"Both email form: ${emailForm._id} and gg form: ${ggForm._id} found."))
      case (Some(emailForm), None) =>
        if (emailForm.status == Submitted) {
          Future.successful(())
        } else {

          val migratedForm = emailForm.copy(
            _id = formIdData.toFormId,
            userId = formIdData.fold(_.userId)(_.userId)
          )
          for {
            _ <- formMetadataService.upsert(formIdData)
            _ <- formMetadataService.delete(emailFormIdData)
            _ <- formPersistence.upsert(migratedForm)
            _ <- formPersistence.delete(emailForm._id)

          } yield ()
        }
      case (None, Some(_)) => Future.successful(())
      case (None, None)    => Future.successful(())
    }

  def migrateEmail(): Action[EmailToGGMigration] =
    Action.async(parse.json[EmailToGGMigration]) { implicit request =>
      val emailToGGMigration: EmailToGGMigration = request.body

      val emailFormId: FormIdData =
        FormIdData.Plain(UserId(emailToGGMigration.emailRetrieval), emailToGGMigration.formTemplateId)
      for {
        emailForm <- formPersistence.find(emailFormId.toFormId)
        ggForm    <- formPersistence.find(emailToGGMigration.formIdData.toFormId)
        _         <- reconcileForms(emailForm, ggForm, emailFormId, emailToGGMigration.formIdData)
      } yield NoContent
    }

}
