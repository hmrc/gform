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

package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FileUploadConnector(config: FUConfig, wSHttp: WSHttp, timeProvider: TimeProvider) {
  val helper = new Helper(config, timeProvider)

  def createEnvelope(formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): Future[EnvelopeId] = {
    val requestBody = helper.createEnvelopeRequestBody(formTemplateId)
    wSHttp
      .POST(s"$baseUrl/file-upload/envelopes", requestBody)
      .map(helper.extractEnvelopId)
  }

  def routeEnvelope(input: RouteEnvelopeRequest)(implicit hc: HeaderCarrier): Future[Unit] = {
    wSHttp
      .POST[RouteEnvelopeRequest, HttpResponse](s"$baseUrl/file-routing/requests", input)
      .map(_ => ())
  }

  private lazy val baseUrl = config.fileUploadBaseUrl
}
