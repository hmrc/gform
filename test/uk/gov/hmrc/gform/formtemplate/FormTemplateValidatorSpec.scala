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

package uk.gov.hmrc.gform.formtemplate

import cats.Eval
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{ Matchers, WordSpecLike }
import parseback.LineStream
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyDate, Date, DateCtx, DateFormCtxVar, FormComponentId, FormCtx, InformationMessage, Instruction, Offset, StandardInfo, Text, TextConstraint, TotalValue }
import parseback.compat.cats._

class FormTemplateValidatorSpec
    extends WordSpecLike with Matchers with FormTemplateSupport with TableDrivenPropertyChecks {

  "validate - for DateCtx" when {
    "expression refers to existing Form field" should {
      "return Valid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(DateCtx(DateFormCtxVar(FormCtx(FormComponentId("someExistingId")))), sections)
        result shouldBe Valid
      }
    }

    "expression refers to non-existing Form field" should {
      "return Invalid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result = FormTemplateValidator
          .validate(DateCtx(DateFormCtxVar(FormCtx(FormComponentId("someNonExistingId")))), sections)
        result shouldBe Invalid("Form field(s) 'someNonExistingId' not defined in form template.")
      }
    }
  }

  "validateInstructions" when {

    "instructions have valid fields" should {
      "return valid" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
              )
            ),
            Some(Instruction(Some(toSmartString("section1Instruction")), Some(1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Valid
      }
    }

    "instructions with no names (section)" should {
      "passes validation" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
              )
            ),
            Some(Instruction(None, None))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Valid
      }
    }

    "instructions have empty names (section)" should {
      "return validation error" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
              )
            ),
            Some(Instruction(Some(toSmartString("")), Some(1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more sections have instruction attribute with empty names")
      }
    }

    "instructions have negative order (section)" should {
      "return validation error" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
              )
            ),
            Some(Instruction(Some(toSmartString("section1Instruction")), Some(-1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more sections have instruction attribute with negative order")
      }
    }

    "instructions in section have duplicate order" should {
      "return validation error" in {
        val page = mkSectionNonRepeatingPage(
          name = "section1",
          formComponents = List(
            mkFormComponent(
              "section1Component1",
              Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
            )
          ),
          Some(Instruction(Some(toSmartString("section1Instruction")), Some(1)))
        ).page
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
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(1)))
              ),
              mkFormComponent(
                "section1Component2",
                Some(Instruction(Some(toSmartString("section1Component2Instruction")), Some(1)))
              )
            ),
            Some(Instruction(Some(toSmartString("section1Instruction")), Some(1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid(
          "All fields within sections that have instruction defined, must have a unique order value"
        )
      }
    }

    "instructions have empty names (field)" should {
      "return validation error" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent("section1Component1", Some(Instruction(Some(toSmartString("")), Some(1))))
            ),
            Some(Instruction(Some(toSmartString("section1Instruction")), Some(1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more section fields have instruction attribute with empty names")
      }
    }

    "instructions have negative order (field)" should {
      "return validation error" in {
        val pages = List(
          mkSectionNonRepeatingPage(
            name = "section1",
            formComponents = List(
              mkFormComponent(
                "section1Component1",
                Some(Instruction(Some(toSmartString("section1Component1Instruction")), Some(-1)))
              )
            ),
            Some(Instruction(Some(toSmartString("section1Instruction")), Some(1)))
          ).page
        )

        val result = FormTemplateValidator.validateInstructions(pages)

        result shouldBe Invalid("One or more section fields have instruction attribute with negative order")
      }
    }
  }

  "validatePeriodFunReferenceConstraints" should {

    "validate references in period function" in {

      val table = Table(
        ("expression", "expectedResult"),
        ("${period(startDate, endDate)}", Valid),
        (
          "${period(startDate, name)}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in period function should be date type"
          )
        ),
        (
          "${period(startDate, name).sum}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in period function should be date type"
          )
        )
      )

      forAll(table) { (expression, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponent("name"),
                mkFormComponent("startDate", Date(AnyDate, Offset(0), None), true),
                mkFormComponent("endDate", Date(AnyDate, Offset(0), None), true)
              )
            ),
            mkSectionNonRepeatingPage(
              name = "section2",
              formComponents = List(
                mkFormComponent(
                  "infoField",
                  InformationMessage(
                    StandardInfo,
                    SmartString(
                      LocalisedString(Map(LangADT.En -> "{0}")),
                      ValueParser.expr(LineStream[Eval](expression)).value.toSeq.flatMap(_.toList).toList
                    )
                  ),
                  true
                )
              )
            )
          )
        )
        val result = FormTemplateValidator.validatePeriodFunReferenceConstraints(formTemplate)
        result shouldBe expectedResult
      }
    }
  }

  "validateSubmitModePresentationHint" should {
    "validate submitMode and presentationHint combinations" in {
      val table = Table(
        ("submitMode", "presentationHint", "expected"),
        (
          "derived",
          List(TotalValue),
          Invalid(
            "Form component derivedName has invalid combination of submitMode (derived) and presentationHint (totalValue)"
          )
        ),
        ("derived", List.empty, Valid),
        ("readOnly", List(TotalValue), Valid),
        ("readOnly", List.empty, Valid)
      )

      forAll(table) { (submitMode, presentationHint, expected) =>
        val derived = submitMode == "derived"
        val editable = submitMode != "readOnly"
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponent("name"),
                mkFormComponent("derivedName", Text(TextConstraint.default, FormCtx(FormComponentId("name"))), editable)
                  .copy(derived = derived, presentationHint = Some(presentationHint))
              )
            )
          )
        )
        val result = FormTemplateValidator.validateSubmitModePresentationHint(formTemplate)
        result shouldBe expected
      }
    }
  }
}
