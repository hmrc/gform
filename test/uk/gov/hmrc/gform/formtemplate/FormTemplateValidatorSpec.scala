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

package uk.gov.hmrc.gform.formtemplate

import org.scalatest.{ Matchers, WordSpecLike }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, Instruction, Page, ShortText, Text, Value }

class FormTemplateValidatorSpec extends WordSpecLike with Matchers {

  "validateInstructions" when {

    "instructions have valid fields" should {
      "return valid" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(toSmartString("section1Component1Instruction"), Some(1))))
            ),
            Some(Instruction(toSmartString("section1Instruction"), Some(1)))
          )
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Valid
      }
    }

    "instructions have empty names (section)" should {
      "return validation error" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(toSmartString("section1Component1Instruction"), Some(1))))
            ),
            Some(Instruction(toSmartString(""), Some(1)))
          )
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more sections have instruction attribute with empty names")
      }
    }

    "instructions have negative order (section)" should {
      "return validation error" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(toSmartString("section1Component1Instruction"), Some(1))))
            ),
            Some(Instruction(toSmartString("section1Instruction"), Some(-1)))
          ),
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more sections have instruction attribute with negative order")
      }
    }

    "instructions in section have duplicate order" should {
      "return validation error" in {
        val page = mkPage(
          name = "section1",
          formComponents = List(
            mkFormComponent(
              "section1Component1",
              Some(Instruction(toSmartString("section1Component1Instruction"), Some(1))))
          ),
          Some(Instruction(toSmartString("section1Instruction"), Some(1)))
        )
        val pages = List(
          page,
          page.copy(title = toSmartString("section2"))
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more sections have instruction attribute with duplicate order value")
      }
    }

    "instructions on fields within a section have duplicate order" should {
      "return validation error" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(toSmartString("section1Component1Instruction"), Some(1)))),
              mkFormComponent(
                "section1Component2",
                Some(Instruction(toSmartString("section1Component2Instruction"), Some(1))))
            ),
            Some(Instruction(toSmartString("section1Instruction"), Some(1)))
          ))

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid(
          "All fields within sections that have instruction defined, must have a unique order value")
      }
    }

    "instructions have empty names (field)" should {
      "return validation error" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent("section1Component1", Some(Instruction(toSmartString(""), Some(1))))
            ),
            Some(Instruction(toSmartString("section1Instruction"), Some(1)))
          ),
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more section fields have instruction attribute with empty names")
      }
    }

    "instructions have negative order (field)" should {
      "return validation error" in {
        val pages = List(
          mkPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(toSmartString("section1Component1Instruction"), Some(-1))))
            ),
            Some(Instruction(toSmartString("section1Instruction"), Some(1)))
          )
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more section fields have instruction attribute with negative order")
      }
    }
  }

  private def mkFormComponent(id: String, instruction: Option[Instruction]) =
    FormComponent(
      FormComponentId(id),
      Text(ShortText.default, Value),
      toSmartString(id),
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None,
      Nil,
      instruction
    )

  private def mkPage(name: String, formComponents: List[FormComponent], instruction: Option[Instruction]) =
    Page(
      toSmartString(name),
      None,
      None,
      None,
      None,
      None,
      formComponents,
      None,
      None,
      instruction
    )
}
