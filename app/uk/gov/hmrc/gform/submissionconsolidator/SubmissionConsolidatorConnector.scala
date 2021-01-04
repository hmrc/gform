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

package uk.gov.hmrc.gform.submissionconsolidator

import play.api.libs.json.Json
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

class SubmissionConsolidatorConnector(wSHttp: WSHttp, baseUrl: String)(implicit ec: ExecutionContext) {

  private val headers = Seq("Content-Type" -> "application/json")

  implicit val reads: HttpReads[Either[String, Unit]] = new HttpReads[Either[String, Unit]] {
    override def read(method: String, url: String, response: HttpResponse): Either[String, Unit] =
      response.status match {
        case 200 => Right(())
        case _   => Left(Try(Json.parse(response.body).as[SCError].formatted).getOrElse(response.body))
      }
  }

  def sendForm(scForm: SCForm)(implicit headerCarrier: HeaderCarrier): Future[Either[String, Unit]] =
    wSHttp
      .POST(s"$baseUrl/submission-consolidator/form", scForm, headers)
      .recover {
        case e => Left(e.getMessage)
      }
}
