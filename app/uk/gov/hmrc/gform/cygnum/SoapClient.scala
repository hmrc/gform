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

package uk.gov.hmrc.gform.cygnum

import cats.Monad
import cats.implicits._
import com.softwaremill.sttp.Id
import uk.gov.hmrc.gform.cygnum.http.{ CygnumClient, CygnumResponse }
import uk.gov.hmrc.gform.cygnum.soap.ProxyCode._

import scala.io.Source
import scala.xml.XML

class SoapClient[F[_]](client: CygnumClient[F]) {

  def retrieveUrn(implicit M: Monad[F]): F[CygnumResponse] = {
    val urnResponse: F[CygnumResponse] =
      client.sendRequest(buildPayload(XML.loadString(GetUrnTemplate.urnTemplate), GetData).getOrElse(""))

    urnResponse.map(
      response =>
        response.body.fold(
          e => e,
          r => println(s"URN: ${new CygnumDataExtractor(new CygnumDataExtractorProgram[Id]).extractUrn(r)}")))

    urnResponse
  }

  def submitForm(implicit M: Monad[F]): F[CygnumResponse] = {

    //TODO hard coded data
    val xml: String =
      Source.fromFile("./ofsted_example_templates/submitFormTemplate.xml").getLines.mkString

    val formResponse: F[CygnumResponse] =
      client.sendRequest(buildPayload(XML.loadString(xml), SendData).getOrElse(""))

    formResponse.map(response => println(s"Status: ${response.status}\nBody: ${response.body}"))

    formResponse
  }
}
