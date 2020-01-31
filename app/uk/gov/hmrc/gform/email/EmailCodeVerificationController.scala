/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.email

import cats.instances.future._
import play.api.mvc.ControllerComponents
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.notifier.{ NotifierAlgebra, NotifierEmail, NotifierEmailReference }
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress

class EmailCodeVerificationController(
  controllerComponents: ControllerComponents,
  notifierAlgebra: NotifierAlgebra[FOpt],
  emailService: EmailService
)(
  implicit ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def sendEmail(notifierEmailAddress: NotifierEmailAddress) =
    Action.async(parse.json[ConfirmationCodeWithEmailService]) { implicit request =>
      val ConfirmationCodeWithEmailService(code, emailVerifierService) = request.body
      emailVerifierService match {
        case EmailVerifierService.Notify(emailTemplateId) =>
          val notifierEmail: NotifierEmail =
            NotifierEmail(
              emailTemplateId,
              notifierEmailAddress,
              Map("confirmation code" -> code.code),
              NotifierEmailReference("")
            )
          notifierAlgebra.email(notifierEmail).map(_ => NoContent).toFuture
        case EmailVerifierService.DigitalContact(emailTemplateId) =>
          emailService
            .sendEmail(
              Some(notifierEmailAddress.value),
              emailTemplateId.value,
              EmailParametersRecalculated(
                Map(EmailTemplateVariable("confirmationCode") -> EmailParameterValue(code.code)))
            )
            .map(_ => NoContent)
      }
    }
}
