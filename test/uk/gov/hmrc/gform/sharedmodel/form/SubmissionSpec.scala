/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.submission.Submission

class SubmissionSpec extends Spec {

  "case class Submission" should "be serialized into json" in new ExampleData {

    override val formFields = super.formFields.take(2)

    val expecteJsonString =
      """
      {
        "_id": "James007-AAA999",
        "submittedDate": "2007-12-03T10:15:30",
        "submissionRef": "DMS",
        "envelopeId": "b66c5979-e885-49cd-9281-c7f42ce6b307",
        "formTemplateId": "AAA999",
        "customerId": "TESTNINO"
      }
      """.stripMargin

    val expectedJson = Json.parse(expecteJsonString)

    Submission.format.writes(submission) shouldBe expectedJson

    Submission.format.reads(expectedJson) shouldBe JsSuccess(submission)
  }
}
