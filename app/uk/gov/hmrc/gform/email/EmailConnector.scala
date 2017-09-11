/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.Logger
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http._

import scala.concurrent.ExecutionContext

class EmailConnector(wsHttp: WSHttp, baseUrl: String, emailEnabled: Boolean) {

  def sendEmail(emailTemplate: EmailTemplate)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Unit = {
    if (emailEnabled) {
      wsHttp.POST[EmailTemplate, HttpResponse](baseUrl + "hmrc/email", emailTemplate)
    } else {
      Logger.info("Sending email disable in this environment")
    }
  }
}
