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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormCtx, Validator }

class ValidatorSpec extends Spec {

  "HMRCUTRPostcodeCheckValidator" should "serialise to JSON correctly" in {
    val x: JsValue = Validator.format.writes(hMRCUTRPostcodeCheckValidator)
    x shouldBe Json.obj(
      "validatorName" -> "hmrcUTRPostcodeCheck",
      "errorMessage"  -> "The UTR could not be foundor the postcode did not match. | <Welsh...>",
      "parameters" -> Json.obj(
        "utr"      -> FormCtx.simpleDollarWrites.writes(hMRCUTRPostcodeCheckValidator.utr),
        "postcode" -> FormCtx.simpleDollarWrites.writes(hMRCUTRPostcodeCheckValidator.postcode)
      )
    )
  }

  it should "parse JSON correctly" in {
    verifyRoundTrip(hMRCUTRPostcodeCheckValidator)
  }

  "BankAccoutnModulusValidator" should "serialise to JSON correctly" in {
    val x: JsValue = Validator.format.writes(bankAccoutnModulusCheckValidator)
    x shouldBe Json.obj(
      "validatorName" -> "bankAccountModulusCheck",
      "errorMessage"  -> "This is an error message for Bank",
      "parameters" -> Json.obj(
        "accountNumber" -> FormCtx.simpleDollarWrites.writes(bankAccoutnModulusCheckValidator.accountNumber),
        "sortCode"      -> FormCtx.simpleDollarWrites.writes(bankAccoutnModulusCheckValidator.sortCode)
      )
    )
  }

  it should "parse JSON correctly" in {
    verifyRoundTrip(bankAccoutnModulusCheckValidator)
  }

  private def verifyRoundTrip[T <: Validator: OFormat](t: T) = {
    val json: JsValue = Json.parse(Validator.format.writes(t).toString)
    val validator = json.as[Validator]
    validator shouldBe t

  }
}
