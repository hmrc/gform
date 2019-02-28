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

package uk.gov.hmrc.gform.form
import org.scalatest.{ MustMatchers, WordSpec }
import uk.gov.hmrc.gform.auditing.Connector
import uk.gov.hmrc.play.audit.http.connector.AuditResult
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.Future

class ConnectorSpec extends WordSpec with MustMatchers {

  "Handle a request" in {

    val body = """"test1": "test1""""

    val stubbedConn = new Connector {
      override val connector: DataEvent => Future[AuditResult] = _ => Future.successful(AuditResult.Success)
      override def logger(msg: String): Unit = msg mustBe body
    }
    val event = DataEvent("src", "type", detail = Map("formId" -> body))

    stubbedConn.sendRequest(event)
  }
}
