/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormatValidationSpec extends Spec {

  behavior of "Text Constraint"

  it should "return Sterling" in createTest("sterling", Sterling)
  it should "return ukBankAccountNumber" in createTest("ukBankAccountNumber", UkBankAccountNumber)
  it should "return ukSortCode" in createTest("ukSortCode", UkSortCode)
  it should "return utr" in createTest("UTR", UTR)
  it should "return nino" in createTest("NINO", NINO)

  def createTest(format: String, constraint: TextConstraint) = {
      val json: JsValue = makeJson(format)
      reads.reads(json) shouldBe JsSuccess(FieldValue(
        id = FieldId(id),
        `type` = Text(constraint, Constant(""),
          false),
        label = label,
        helpText = None,
        shortName = None,
        mandatory = true,
        editable = true,
        submissible = true,
        errorMessage = None,
        presentationHint = None
      ))
    }

  "Text Constraint" should "return ukBankAccountNumber" in {
    val json: JsValue = makeJson("ukBankAccountNumber")
    reads.reads(json) shouldBe JsSuccess(FieldValue(
      id = FieldId(id),
      `type` = Text(UkBankAccountNumber, Constant(""),
        false),
      label = label,
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None,
      presentationHint = None
    ))
  }

  "Text Constraint" should "return ukSortCode" in {
    val json: JsValue = makeJson("ukSortCode")
    reads.reads(json) shouldBe JsSuccess(FieldValue(
      id = FieldId(id),
      `type` = Text(UkSortCode, Constant(""),
        false),
      label = label,
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None,
      presentationHint = None
    ))
  }
  "Text Constraint" should "return telephoneNumber" in {
    val json: JsValue = makeJson("telephoneNumber")
    reads.reads(json) shouldBe JsSuccess(FieldValue(
      id = FieldId(id),
      `type` = Text(TelephoneNumber, Constant(""),
        false),
      label = label,
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None,
      presentationHint = None
    ))
  }
  "Text Constraint" should "return email" in {
    val json: JsValue = makeJson("email")
    reads.reads(json) shouldBe JsSuccess(FieldValue(
      id = FieldId(id),
      `type` = Text(Email, Constant(""),
        false),
      label = label,
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None,
      presentationHint = None
    ))
  }

  private def makeJson(format: String) = Json.parse(getJson(format))

  lazy val id = """alcIngDescription"""
  lazy val label = """Alcohol Ingredients"""

  def getJson(format: String): String = {
    s"""{
         "id": "$id",
         "type": "text",
         "format": "$format",
         "label": "$label"
        }
      """
  }

}
