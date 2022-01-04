/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen

class FormComponentSpec extends Spec with ScalaCheckDrivenPropertyChecks {
  "FormComponent" should "round trip derived JSON" in {
    forAll(FormComponentGen.formComponentGen()) { value =>
      FormComponent.format.reads(FormComponent.format.writes(value)) should beJsSuccess(value)
    }
  }

  "formComponentId" should "return a JsError when validated if it contains a hyphen" in {
    val invalidStr = "foo-bar"
    verifyError(invalidStr)
  }

  it should "return a JsSuccess when given an id which does not start with a number and doesn't contain any special " +
    "characters, apart from underscores." in {
      forAll(FormComponentGen.formComponentIdGen) { input =>
        formIdValidator(input.value) should beJsSuccess(input)
      }
    }

  it should "return a JsError when validated if it begins with a number" in {
    forAll(FormComponentGen.idBeginningWithNumberGen) { input =>
      verifyError(input.value)
    }
  }

  private def formIdValidator(s: String) = FormComponentId.validate(s)
  private def verifyError(input: String) = formIdValidator(input) should beJsError(FormComponentId.errorMessage(input))

}
