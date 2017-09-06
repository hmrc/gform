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

import play.api.libs.json.{ JsSuccess, JsValue, Json }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormCtx, HMRCUTRPostcodeCheckValidator, Validator }

class ValidatorSpec extends Spec {

  "Validator" should "return HMRCUTRPostcodeCheckValidator " in {

    val json: JsValue = Json.parse(s"""{
                              "validatorName": "hmrcUTRPostcodeCheck",
                              "errorMessage": "${hMRCUTRPostcodeCheckValidator.errorMessage}",
                              "parameters": [
                                  {"utr": "${"$" + hMRCUTRPostcodeCheckValidator.utr.value}"},
                                  {"postcode": "${"$" + hMRCUTRPostcodeCheckValidator.postcode.value}"}
                              ]
                            }""")

    val validator = json.as[Validator]
    validator shouldBe hMRCUTRPostcodeCheckValidator
    val x: JsValue = Validator.format.writes(validator)
    x shouldBe Json.obj(
      "validatorName" -> "hmrcUTRPostcodeCheck",
      "errorMessage" -> "The UTR could not be foundor the postcode did not match. | <Welsh...>",
      "utr" -> Json.obj("value" -> hMRCUTRPostcodeCheckValidator.utr.value),
      "postcode" -> Json.obj("value" -> hMRCUTRPostcodeCheckValidator.postcode.value)
    )
  }


  "Validator" should "return BankAccoutnModulusCheck " in {

    val json: JsValue = Json.parse(s"""{
                              "validatorName": "bankAccoutnModulusCheck",
                              "errorMessage": "${bankAccoutnModulusCheckValidator.errorMessage}",
                              "parameters": [
                                  {"accountNumber": "${"$" + bankAccoutnModulusCheckValidator.accountNumber.value}"},
                                  {"sortCode": "${"$" + bankAccoutnModulusCheckValidator.sortCode.value}"}
                              ]
                            }""")

    val validator = json.as[Validator]
    validator shouldBe bankAccoutnModulusCheckValidator
    val x: JsValue = Validator.format.writes(validator)
    x shouldBe Json.obj(
      "validatorName" -> "bankAccoutnModulusCheck",
      "errorMessage" -> "This is an error message for Bank",
      "accountNumber" -> Json.obj("value" -> bankAccoutnModulusCheckValidator.accountNumber.value),
      "sortCode" -> Json.obj("value" -> bankAccoutnModulusCheckValidator.sortCode.value)
    )
  }


}
