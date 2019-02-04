/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.connectors

import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

class PdfGeneratorConnector(servicesConfig: ServicesConfig, wSHttp: WSHttp) {

  //TODO: use stream
  def generatePDF(payload: Map[String, Seq[String]], headers: Seq[(String, String)])(
    implicit hc: HeaderCarrier): FOpt[Array[Byte]] = {
    Logger.info(s"generate pdf, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    val url = s"$baseURL/pdf-generator-service/generate"
    fromFutureOptA {
      wSHttp.buildRequest(url).withHeaders(headers: _*).post(payload).map { response =>
        val status = response.status
        if (status >= 200 && status < 300) {
          Right(response.bodyAsBytes.toArray)
        } else {
          Left(UnexpectedState(s"POST to $url failed with status $status. Response body: '${response.body}'"))
        }
      }
    }
  }

  private val serviceName = "pdf-generator"
  lazy val baseURL = servicesConfig.baseUrl(serviceName) + servicesConfig.getConfString(s"$serviceName.base-path", "")
}
