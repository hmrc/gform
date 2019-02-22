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

import cats.Id
import com.google.gson.JsonObject
import org.scalatest.{ MustMatchers, WordSpec }
import uk.gov.hmrc.gform.auditing.EventAudit
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.play.audit.http.connector.AuditResult
import uk.gov.hmrc.play.audit.http.connector.AuditResult.{ Failure, Success }
import uk.gov.hmrc.play.audit.model
import uk.gov.hmrc.play.audit.model.{ DataEvent, _ }

class FormControllerRequestHandlerSpec extends WordSpec with MustMatchers {

  "Handle a request" in {
    val eventAudit: EventAudit[Id] = new EventAudit[Id] {
      override def program(event: DataEvent) = Success
    }

    val handler = new FormControllerRequestHandler[Id](eventAudit)
    val event = DataEvent("src", "type")

    handler.handleRequest(event) mustBe AuditResult.Success
  }

  "Logs an AuditResult failure" in {
    val failureMsg = "something wrong"

    val eventAudit: EventAudit[Id] = new EventAudit[Id] {
      override def logger(msg: String) =
        msg must be(failureMsg)
    }
    eventAudit.sideEffect(Failure(failureMsg)) must be(())
  }

  "Converts body and form id to DataEvent" in {
    val body = """|{
                      "test1": "test1",
                      "test2": "test2",
                      "test3": "test3"
                    }"""

    val eventAudit: EventAudit[Id] = new EventAudit[Id]

    val handler = new FormControllerRequestHandler[Id](eventAudit)

    val result = handler.requestToDataEvent(FormId("formId"), body)

    result.auditSource mustBe "Gform"
    result.auditType mustBe "enrolmentCallback"
    result.detail mustBe Map("formId" -> body)

  }

}
