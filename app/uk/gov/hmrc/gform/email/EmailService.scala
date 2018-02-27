/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.form.Form
import cats.implicits._
import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class EmailService(emailConnector: EmailConnector) {
  def sendEmail(optemailAddress: Option[String], templateId: String)(implicit hc: HeaderCarrier, mdc: ExecutionContext): Future[Unit] = {
    Logger.info(s" Sending email, template: ${templateId}, headers: '${loggingHelpers.cleanHeaderCarrierHeader(hc)}'")
    optemailAddress.fold(().pure[Future])(email => emailConnector.sendEmail(new EmailTemplate(Seq(email), templateId)))
  }
  def getEmailAddress(form: Form): Option[String] =
    form.formData.fields.map(x => x.id.value -> x.value).toMap.get("email")
}
