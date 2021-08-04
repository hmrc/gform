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

package uk.gov.hmrc.gform.sharedmodel

import org.scalactic.source.Position
import org.scalatest.{ Assertion, FlatSpec, Matchers }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.JsResultMatcher
import uk.gov.hmrc.gform.formtemplate.EmailVerification
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierTemplateId

class EmailVerificationSpec extends FlatSpec with Matchers with JsResultMatcher {

  import EmailVerification._
  import EmailVerifierService._

  "EmailVerification" should "parse email verification with Notify service" in {
    val jsonPayload = """|{
                         |  "service": "notify",
                         |  "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4",
                         |  "codeField": "someFieldId"
                         |}
                         |"""

    val expected =
      VerifiedBy(
        FormComponentId("someFieldId"),
        Notify(NotifierTemplateId("4f438fe6-680d-4610-9e55-b50f711326e4"), None)
      )
    verifyPayload(jsonPayload, expected)

  }
  it should "parse email verification with DigitalContact service" in {
    val jsonPayload = """|{
                         |  "service": "digitalContact",
                         |  "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4",
                         |  "codeField": "someFieldId"
                         |}
                         |"""

    val expected = VerifiedBy(
      FormComponentId("someFieldId"),
      DigitalContact(EmailTemplateId("4f438fe6-680d-4610-9e55-b50f711326e4"), None)
    )

    verifyPayload(jsonPayload, expected)
  }

  it should "fail when value of 'service' is wrong" in {
    val jsonPayload = """|{
                         |  "service": "wrongService",
                         |  "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4",
                         |  "codeField": "someFieldId"
                         |}
                         |"""

    verifyWrongPayload(jsonPayload, """Unsupported email service '"wrongService"'""")
  }

  it should "fail when 'service' field is missing" in {
    val jsonPayload = """|{
                         |  "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4",
                         |  "codeField": "someFieldId"
                         |}
                         |"""

    verifyWrongPayload(jsonPayload, "Missing email service. Specify one of 'notify' or 'digitalContact'")
  }

  it should "fail when 'emailTemplateId' field is missing" in {
    val jsonPayload = """|{
                         |  "service": "digitalContact",
                         |  "codeField": "someFieldId"
                         |}
                         |"""

    verifyWrongPayload(jsonPayload, "Missing field 'emailTemplateId' in json")
  }
  it should "fail when 'codeField' field is missing" in {
    val jsonPayload = """|{
                         |  "service": "digitalContact",
                         |  "emailTemplateId": "4f438fe6-680d-4610-9e55-b50f711326e4"
                         |}
                         |"""

    verifyWrongPayload(jsonPayload, "Missing field 'codeField' in json")
  }

  private def verifyPayload(jsonPayload: String, emailVerification: EmailVerification)(implicit
    position: Position
  ): Assertion = {
    val json = Json.parse(jsonPayload.stripMargin)

    val jsResult = EmailVerification.reads.reads(json)

    jsResult should beJsSuccess(emailVerification)
  }

  private def verifyWrongPayload(jsonPayload: String, error: String)(implicit position: Position): Assertion = {
    val json = Json.parse(jsonPayload.stripMargin)

    val jsResult = EmailVerification.reads.reads(json)

    jsResult should beJsError(error)
  }
}
