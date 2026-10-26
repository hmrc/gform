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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.formtemplate.FormTemplateSupport
import uk.gov.hmrc.gform.sharedmodel.generators.FormComponentValidatorGen

class FormComponentValidatorSpec extends Spec with ScalaCheckDrivenPropertyChecks with FormTemplateSupport {
  "FormComponentValidator" should "round trip derived JSON" in {
    forAll(FormComponentValidatorGen.formComponentValidatorGen) { obj =>
      verifyRoundTrip(obj)
    }
  }

  it should "read valid form component templates JSON" in {
    val errorMessage = toSmartString("sometext")
    val json = """{"validIf" : "${hallo='goobye'}", "errorMessage": "sometext"}"""
    val validator =
      FormComponentValidator(ValidIf(Equals(FormCtx(FormComponentId("hallo")), Constant("goobye"))), errorMessage)
    verifyRead(validator, json)
  }

  "PageValidator" should "serialize member IDs as strings and preserve the localized error message" in {
    val validator = PageValidator(
      ValidIf(Equals(FormCtx(FormComponentId("first")), FormCtx(FormComponentId("second")))),
      NonEmptyList.of(FormComponentId("first"), FormComponentId("second")),
      toSmartString("The values must match", "Rhaid cyfateb")
    )
    val json = Json.toJson(validator)
    (json \ "fieldIds").as[List[String]] shouldBe List("first", "second")
    json.as[PageValidator] shouldBe validator
    verifyRead(
      validator,
      """{"validIf":"${first=second}","fieldIds":["first","second"],"errorMessage":{"en":"The values must match","cy":"Rhaid cyfateb"}}"""
    )
  }

  it should "reject invalid form component IDs in page validator JSON" in {
    List("", "1first", "first-second").foreach { invalidId =>
      val json = Json.obj(
        "validIf"      -> "${first=second}",
        "fieldIds"     -> List("first", invalidId),
        "errorMessage" -> "The values must match"
      )

      json.validate[PageValidator].isError shouldBe true
    }
  }

  it should "reject an empty fieldIds array" in {
    val json = Json.obj(
      "validIf"      -> "${first > 10}",
      "fieldIds"     -> Json.arr(),
      "errorMessage" -> "Enter more than 10"
    )

    json.validate[PageValidator].isError shouldBe true
  }

  it should "read and round trip a single fieldId for subsequent minimum-two-fields validation" in {
    val validator = PageValidator(
      ValidIf(IsTrue),
      NonEmptyList.one(FormComponentId("first")),
      toSmartString("Enter more than 10")
    )

    verifyRoundTrip(validator)
    verifyRead(
      validator,
      """{"validIf":"${true}","fieldIds":["first"],"errorMessage":"Enter more than 10"}"""
    )
  }

  "Page validators" should "round trip non-empty arrays and allow an absent property" in {
    val page = mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "first"))).page
    val validator = PageValidator(
      ValidIf(IsTrue),
      NonEmptyList.of(FormComponentId("first"), FormComponentId("second")),
      toSmartString("The values must match")
    )

    Json.toJson(page).as[Page].validators shouldBe None
    List(NonEmptyList.one(validator), NonEmptyList.of(validator, validator)).foreach { validators =>
      val withValidators = page.copy(validators = Some(validators))
      val json = Json.toJson(withValidators)

      (json \ "validators").as[List[PageValidator]] shouldBe validators.toList
      json.as[Page] shouldBe withValidators
    }
  }

  it should "reject an empty validators array" in {
    val page = mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "first"))).page
    val json = Json.toJsObject(page) + ("validators" -> Json.arr())

    json.validate[Page].isError shouldBe true
  }
}
