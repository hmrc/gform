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

package uk.gov.hmrc.gform.cygnum.http

import cats.Monad
import com.softwaremill.sttp.quick.{ emptyRequest, _ }
import com.softwaremill.sttp.{ Id, RequestT, Response }
import uk.gov.hmrc.gform.config.CygnumConfig
import uk.gov.hmrc.gform.cygnum.CygnumTemplate._
import uk.gov.hmrc.gform.cygnum.soap.ProxyCode.buildPayload
import uk.gov.hmrc.gform.cygnum.{ GetData, SendData, ServiceName }
import uk.gov.hmrc.http.HttpResponse

import scala.xml.XML

class CygnumClient[F[_]] extends CygnumConfig {

  def sendRequest(serviceName: ServiceName, payload: String)(implicit M: Monad[F]): F[HttpResponse] = {
    val requestBody = buildPayload(XML.loadString(wrapper(serviceName, payload)), serviceName).getOrElse("")

    val soapRequest: RequestT[Id, String, Nothing] = emptyRequest
      .post(uri"$cygnumURL")
      .body(requestBody)
      .header("Content-Type", "application/soap+xml; charset=utf-8", true)

    val response: Id[Response[String]] = soapRequest.send()

//    println(s"\n\n\n\nRequest:\n$requestBody\n\n\n\n\n")

    println(s"\n\n\n\nResponse:\n$response\n\n\n\n\n")

    M.pure(response.body.fold(_ => buildResponse(response, None), body => buildResponse(response, Some(body))))
  }

  private def buildResponse(response: Response[String], body: Option[String]) =
    HttpResponse(response.code, responseString = body)

  private def wrapper(serviceName: ServiceName, payload: String): String = serviceName match {
    case GetData  => urnTemplate
    case SendData => formSubmissionTemplate(payload)
  }
}
