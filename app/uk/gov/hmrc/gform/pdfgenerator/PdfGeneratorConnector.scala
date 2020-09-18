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

package uk.gov.hmrc.gform.connectors

import play.api.Logger
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class PdfGeneratorConnector(servicesConfig: ServicesConfig, wSHttp: WSHttp)(implicit ex: ExecutionContext) {

  def generatePDF(payload: Map[String, Seq[String]], headers: Seq[(String, String)])(
    implicit hc: HeaderCarrier): Future[Array[Byte]] = {
    val url = s"$baseURL/pdf-generator-service/generate"

    val payloadSize = payload.foldLeft(0) { case (acc, (key, value)) => acc + key.size + value.map(_.size).sum }
    Logger.info(s"Generate pdf. Html payload size is: $payloadSize bytes.")
    wSHttp.buildRequest(url).withHttpHeaders(headers: _*).post(payload).flatMap { response =>
      {
        val status = response.status
        if (status >= 200 && status < 300) {
          Future.successful(response.bodyAsBytes.toArray)
        } else {
          Future.failed(new Exception(s"POST to $url failed with status $status. Response body: '${response.body}'"))
        }
      }
    }
  }

  private val serviceName = "pdf-generator"
  lazy val baseURL = servicesConfig.baseUrl(serviceName) + servicesConfig.getConfString(s"$serviceName.base-path", "")
}
