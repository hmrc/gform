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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpecLike
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.core.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyDate, Date, DateCtx, DateFormCtxVar, ExprWithPath, FormComponentId, FormCtx, InformationMessage, Instruction, LeafExpr, LinkCtx, Offset, PageId, StandardInfo, TemplatePath }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.CsvCountryCheck
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IsTrue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentValidator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ValidIf
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Constant
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IfElse

class FormTemplateValidatorSpec
    extends AnyWordSpecLike with Matchers with FormTemplateSupport with TableDrivenPropertyChecks {

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
                      ValueParser.validateWithParser(expression, ValueParser.expr).right.toSeq.toList
                    )
                  ),
                  true
                )
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validatePeriodFunReferenceConstraints(formTemplate, allExpressions)
        result shouldBe expectedResult
      }
    }
  }

  "validateUniquePageIds" should {
    "validate page ids are unique" in {
      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkSectionRepeatingPage(
              name = "page2",
              formComponents = List.empty,
              pageId = Some(PageId("page2"))
            ),
            mkAddToList(
              name = "page3",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page4",
                  formComponents = List.empty,
                  pageId = Some(PageId("page4"))
                ).page
              )
            )
          ),
          Valid
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkSectionRepeatingPage(
              name = "page2",
              formComponents = List.empty,
              pageId = Some(PageId("page2"))
            ),
            mkAddToList(
              name = "page3",
              defaultPage = Some(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page1"))
                ).page
              ),
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page3",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Invalid("Some page ids are defined more than once: page1,page2")
        )
      )
      forAll(table) { (sections, expected) =>
        FormTemplateValidator.validateUniquePageIds(sections) shouldBe expected
      }
    }
  }

  "validateInvalidReferences" should {
    "validate and report non-existent page id references" in {
      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkSectionRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkSectionNonRepeatingPage(
              name = "page2",
              formComponents = List(
                mkFormComponent(
                  "page2Comp1",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(LinkCtx(PageLink(PageId("page1")))))
                  ),
                  false
                )
              )
            )
          ),
          Valid
        ),
        (
          List(
            mkSectionRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkSectionNonRepeatingPage(
              name = "page2",
              formComponents = List(
                mkFormComponent(
                  "page2Comp1",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(LinkCtx(PageLink(PageId("invalid")))))
                  ),
                  false
                )
              )
            )
          ),
          Invalid("sections.fields.[id=page2Comp1].infoText: Page id 'invalid' doesn't exist in the form")
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkAddToList(
              name = "page2",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List(
                    mkFormComponent(
                      "page2Comp1",
                      InformationMessage(
                        StandardInfo,
                        SmartString(toLocalisedString("{0}"), List(LinkCtx(PageLink(PageId("invalid")))))
                      ),
                      false
                    )
                  ),
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Invalid("sections.pages.fields.[id=page2Comp1].infoText: Page id 'invalid' doesn't exist in the form")
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkAddToList(
              name = "page2",
              pageIdToDisplayAfterRemove = Some(PageId("invalid")),
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List(
                    mkFormComponent(
                      "page2Comp1",
                      InformationMessage(
                        StandardInfo,
                        SmartString(toLocalisedString("foo"), List())
                      ),
                      false
                    )
                  ),
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Invalid("PageId(invalid): doesn't exist in the form")
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List.empty,
              pageId = Some(PageId("page1"))
            ),
            mkAddToList(
              name = "page2",
              pageIdToDisplayAfterRemove = Some(PageId("page1")),
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List(
                    mkFormComponent(
                      "page2Comp1",
                      InformationMessage(
                        StandardInfo,
                        SmartString(toLocalisedString("foo"), List())
                      ),
                      false
                    )
                  ),
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Valid
        )
      )
      forAll(table) { (sections, expected) =>
        FormTemplateValidator.validateInvalidReferences(mkFormTemplate(sections)) shouldBe expected
      }
    }
  }

  "validate - for CsvCountryCheck" when {
    "expression refers to existing Form field" should {
      "return Valid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(CsvCountryCheck(FormComponentId("someExistingId"), "column"), sections)
        result shouldBe Valid
      }
    }

    "expression refers to non-existing Form field" should {
      "return Invalid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(CsvCountryCheck(FormComponentId("someNonExistingId"), "column"), sections)
        result shouldBe Invalid("Form field 'someNonExistingId' is not defined in form template.")
      }
    }
  }

  "validateErrorMessageConstraints" should {

    "errorMessage in NonRepeatingPage when noPII is false" in {

      val table = Table(
        ("errorMessage", "expectedResult"),
        (toSmartString("no references"), Valid),
        (
          SmartString(toLocalisedString("{0}"), List(FormCtx(FormComponentId("fcId1")))),
          Invalid("sections.fields.[id=fcId2].errorMessage contains PII fcId: fcId1")
        )
      )

      forAll(table) { (errorMessage, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponentWithNotPII(id = "fcId1", notPII = false)
              )
            ),
            mkSectionNonRepeatingPage(
              name = "section2",
              formComponents = List(
                mkFormComponentWithErrorMessage(id = "fcId2", errorMessage = Some(errorMessage))
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateErrorMessageConstraints(formTemplate, allExpressions)
        result shouldBe expectedResult
      }
    }

    "errorMessage in NonRepeatingPage when noPII is true" in {

      val table = Table(
        ("errorMessage", "expectedResult"),
        (toSmartString("no references"), Valid),
        (SmartString(toLocalisedString("{0}"), List(FormCtx(FormComponentId("fcId1")))), Valid)
      )

      forAll(table) { (errorMessage, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponentWithNotPII(id = "fcId1", notPII = true)
              )
            ),
            mkSectionNonRepeatingPage(
              name = "section2",
              formComponents = List(
                mkFormComponentWithErrorMessage(id = "fcId2", errorMessage = Some(errorMessage))
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateErrorMessageConstraints(formTemplate, allExpressions)
        result shouldBe expectedResult
      }
    }

    "validators in NonRepeatingPage when noPII is false" in {

      val table = Table(
        ("validator", "expectedResult"),
        (FormComponentValidator(ValidIf(IsTrue), toSmartString("no references")), Valid),
        (
          FormComponentValidator(
            ValidIf(IsTrue),
            SmartString(
              toLocalisedString("{0}"),
              List(IfElse(IsTrue, FormCtx(FormComponentId("fcId1")), Constant("bar")))
            )
          ),
          Invalid("sections.fields.[id=fcId2].validators.errorMessage contains PII fcId: fcId1")
        )
      )

      forAll(table) { (validator, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponentWithNotPII(id = "fcId1", notPII = false)
              )
            ),
            mkSectionNonRepeatingPage(
              name = "section2",
              formComponents = List(
                mkFormComponentWithValidators(id = "fcId2", validators = validator :: Nil)
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateErrorMessageConstraints(formTemplate, allExpressions)
        result shouldBe expectedResult
      }
    }

    "validators in NonRepeatingPage when noPII is true" in {

      val table = Table(
        ("validator", "expectedResult"),
        (FormComponentValidator(ValidIf(IsTrue), toSmartString("no references")), Valid),
        (
          FormComponentValidator(
            ValidIf(IsTrue),
            SmartString(
              toLocalisedString("{0}"),
              List(IfElse(IsTrue, FormCtx(FormComponentId("fcId1")), Constant("bar")))
            )
          ),
          Valid
        )
      )

      forAll(table) { (validator, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
                mkFormComponentWithNotPII(id = "fcId1", notPII = true)
              )
            ),
            mkSectionNonRepeatingPage(
              name = "section2",
              formComponents = List(
                mkFormComponentWithValidators(id = "fcId2", validators = validator :: Nil)
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateErrorMessageConstraints(formTemplate, allExpressions)
        result shouldBe expectedResult
      }
    }
  }
}
