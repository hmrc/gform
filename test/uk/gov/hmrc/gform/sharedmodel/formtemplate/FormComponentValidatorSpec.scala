/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.generators.FormComponentValidatorGen

class FormComponentValidatorSpec extends Spec {
  "FormComponentValidator" should "round trip derived JSON" in {
    forAll(FormComponentValidatorGen.formComponentValidatorGen) { obj =>
      verifyRoundTrip(obj)
    }
  }

  it should "read valid form component templates JSON" in {
    val errorMessage = toSmartString("sometext")
    val json = """{"validIf" : "${hallo='goobye'}", "errorMessage": "sometext"}"""
    val validator = FormComponentValidator(ValidIf(Equals(FormCtx("hallo"), Constant("goobye"))), errorMessage)
    verifyRead(validator, json)
  }
}
