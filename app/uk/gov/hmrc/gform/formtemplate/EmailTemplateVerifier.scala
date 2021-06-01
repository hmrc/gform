/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.core.{ FOpt, fromFutureOptA }
import uk.gov.hmrc.gform.email._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailParameter, FormTemplate }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class EmailTemplateVerifier(hmrcEmailRendererConnector: HMRCEmailRendererConnector)(implicit ec: ExecutionContext) {
  def verifyEmailTemplate(formTemplate: FormTemplate)(implicit hc: HeaderCarrier): FOpt[Unit] = {
    val emailTemplateParams = formTemplate.emailParameters.fold(Map.empty[String, String])(_.map {
      case EmailParameter(emailTemplateVariable, _) => (emailTemplateVariable, emailTemplateVariable)
    }.toList.toMap)
    fromFutureOptA[Unit](
      hmrcEmailRendererConnector
        .renderTemplate(EmailRenderRequest(formTemplate.emailTemplateId, emailTemplateParams))
        .map {
          case Successful                 => Right(())
          case NotFound                   => Left(UnexpectedState(s"Email template '${formTemplate.emailTemplateId}' not found"))
          case ParametersNotFound(reason) => Left(UnexpectedState(reason))
          case Unexpected(reason)         => Left(UnexpectedState(reason))
        }
    )
  }
}
