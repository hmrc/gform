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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ JsString, JsValue, Json, OFormat }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormCtx, Validator }

class ValidatorSpec extends Spec {

  "HmrcRosmRegistrationCheckValidator" should "Write and Read default JSON correctly" in {
    verifyRoundTrip(hMRCUTRPostcodeCheckValidator)
  }

  it should "parse custom JSON correctly" in {
    Json
      .obj(
        "validatorName" -> "hmrcRosmRegistrationCheck",
        "errorMessage"  -> hMRCUTRPostcodeCheckValidator.errorMessage,
        "parameters" -> Json.obj(
          "regime"   -> "ITSA",
          "utr"      -> customFormCtxJson(hMRCUTRPostcodeCheckValidator.utr),
          "postcode" -> customFormCtxJson(hMRCUTRPostcodeCheckValidator.postcode)
        )
      )
      .as[Validator] shouldBe hMRCUTRPostcodeCheckValidator
  }

  "BankAccoutnModulusValidator" should "Write and Read default JSON correctly" in {
    verifyRoundTrip(bankAccoutnModulusCheckValidator)
  }

  it should "parse custom JSON correctly" in {
    Json
      .obj(
        "validatorName" -> "bankAccountModulusCheck",
        "errorMessage"  -> bankAccoutnModulusCheckValidator.errorMessage,
        "parameters" -> Json.obj(
          "accountNumber" -> customFormCtxJson(bankAccoutnModulusCheckValidator.accountNumber),
          "sortCode"      -> customFormCtxJson(bankAccoutnModulusCheckValidator.sortCode)
        )
      )
      .as[Validator] shouldBe bankAccoutnModulusCheckValidator
  }

  private def verifyRoundTrip[T <: Validator: OFormat](t: T) = {
    val json: JsValue = Json.parse(Validator.format.writes(t).toString)
    val validator = json.as[Validator]
    validator shouldBe t
  }

  private def customFormCtxJson(fc: FormCtx): JsString = JsString(s"""$${${fc.value}}""")
}
