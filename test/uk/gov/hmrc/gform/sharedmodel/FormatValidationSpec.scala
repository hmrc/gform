/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormatValidationSpec extends Spec {

  behavior of "Text Constraint"

  it should "return Sterling with positiveOnly=false" in createTest(
    "sterling",
    Sterling(RoundingMode.defaultRoundingMode, false)
  )
  it should "return Sterling with positiveOnly=true" in createTest(
    "positiveSterling",
    Sterling(RoundingMode.defaultRoundingMode, true)
  )
  it should "return WholeSterling with positive=true" in createTest(
    "positiveWholeSterling",
    WholeSterling(true)
  )
  it should "return ukBankAccountNumber" in createTest("ukBankAccountNumber", UkBankAccountNumber)
  it should "return telephoneNumber" in createTest("telephoneNumber", TelephoneNumber)
  it should "return email" in createTest("email", Email)
  it should "return saUtr" in createTest("saUtr", SaUTR)
  it should "return ctUtr" in createTest("ctUtr", CtUTR)
  it should "return nino" in createTest("nino", NINO)
  it should "return TextWithRestrictions" in createTest("text(1, 1)", TextWithRestrictions(1, 1))
  it should "return ShortText" in createTest("shortText", ShortText.default)
  it should "return UkVrn" in createTest("ukVrn", UkVrn)
  it should "return countryCode" in createTest("countryCode", CountryCode)
  it should "return nonUkCountryCode" in createTest("nonUkCountryCode", NonUkCountryCode)
  it should "return payeReference" in createTest("payeReference", PayeReference)

  it should "return CompanyRegistrationNumber" in createTest("companyRegistrationNumber", CompanyRegistrationNumber)

  it should "return EORI" in createTest("EORI", EORI)

  it should "return UkEORI" in createTest("UkEORI", UkEORI)

  it should "return ChildBenefitNumber" in createTest("childBenefitNumber", ChildBenefitNumber)

  def createTest(format: String, constraint: TextConstraint) = {
    val json: JsValue = makeJson(format)
    reads.reads(json) shouldBe JsSuccess(
      FormComponent(
        id = FormComponentId(id),
        `type` = Text(constraint, Value),
        label = toSmartString(label),
        isPageHeading = false,
        helpText = None,
        shortName = None,
        includeIf = None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None,
        presentationHint = None
      )
    )
  }

  private def makeJson(format: String) = Json.parse(getJson(format))

  lazy val id = """alcIngDescription"""
  lazy val label = """Alcohol Ingredients"""

  def getJson(format: String): String =
    s"""{
         "id": "$id",
         "type": "text",
         "format": "$format",
         "label": "$label"
        }
      """
  val reads: Reads[FormComponent] = implicitly[Reads[FormComponent]]
}
