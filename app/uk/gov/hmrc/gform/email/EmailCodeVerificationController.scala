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

package uk.gov.hmrc.gform.email

import cats.instances.future._
import play.api.mvc.ControllerComponents
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.notifier.{ NotifierAlgebra, NotifierEmail, NotifierEmailReference }
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameterValue, EmailParametersRecalculated, EmailTemplateVariable }
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.EmailCodeParameter

class EmailCodeVerificationController(
  controllerComponents: ControllerComponents,
  notifierAlgebra: NotifierAlgebra[FOpt],
  emailService: EmailService,
  formTemplateService: FormTemplateService
)(implicit
  ex: ExecutionContext
) extends BaseController(controllerComponents) {

  def sendEmail() =
    Action.async(parse.json[ConfirmationCodeWithEmailService]) { implicit request =>
      val ConfirmationCodeWithEmailService(notifierEmailAddress, code, emailVerifierService, lang, formTemplateId) =
        request.body
      formTemplateService
        .get(formTemplateId)
        .flatMap { formTemplate =>
          val personalization = getPersonalization(formTemplate.emailCodeParameters, lang)
          emailVerifierService match {
            case notify @ EmailVerifierService.Notify(_, _) =>
              val notifierEmail: NotifierEmail =
                NotifierEmail(
                  notify.notifierTemplateId(lang),
                  notifierEmailAddress,
                  Map("confirmation code" -> code.code) ++ personalization,
                  NotifierEmailReference("")
                )
              notifierAlgebra.email(notifierEmail).map(_ => NoContent).toFuture
            case dc @ EmailVerifierService.DigitalContact(_, _) =>
              emailService
                .sendEmail(
                  Some(notifierEmailAddress.value),
                  dc.emailTemplateId(lang),
                  EmailParametersRecalculated(
                    (Map("confirmationCode" -> code.code) ++
                      personalization).map { case (k, v) => (EmailTemplateVariable(k), EmailParameterValue(v)) }
                  )
                )
                .map(_ => NoContent)
          }
        }
    }

  private def getPersonalization(
    emailCodeParameters: Option[NonEmptyList[EmailCodeParameter]],
    lang: LangADT
  ): Map[String, String] =
    emailCodeParameters
      .fold(List.empty[EmailCodeParameter])(_.toList)
      .map { case EmailCodeParameter(emailTemplateVariable, value) =>
        (emailTemplateVariable, value.m.getOrElse(lang, ""))
      }
      .toMap
}
