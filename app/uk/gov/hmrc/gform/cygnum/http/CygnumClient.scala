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
import uk.gov.hmrc.gform.config.CygnumConfig
import uk.gov.hmrc.gform.cygnum.CygnumTemplate._
import uk.gov.hmrc.gform.cygnum.soap.ProxyCode.buildPayload
import uk.gov.hmrc.gform.cygnum.{ GetData, SendData, ServiceName }

import scala.xml.XML

class CygnumClient[F[_]] extends CygnumConfig {

  def sendRequest(serviceName: ServiceName, payload: String)(implicit M: Monad[F]): String =
    buildPayload(XML.loadString(wrapper(serviceName, payload)), serviceName).getOrElse("")

  private def wrapper(serviceName: ServiceName, payload: String): String = serviceName match {
    case GetData  => urnTemplate
    case SendData => formSubmissionTemplate(payload)
  }
}
