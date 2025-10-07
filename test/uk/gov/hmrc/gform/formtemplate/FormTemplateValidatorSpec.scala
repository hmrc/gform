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
import play.api.libs.json.{ JsError, JsSuccess, Json }
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.core.{ Invalid, Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyDate, BulletedList, Checkbox, Choice, ChoicesAvailable, ChoicesSelected, Constant, DataRetrieveCtx, Date, DateCtx, DateFormCtxVar, DateFunction, DateProjection, DateValueExpr, DisplayAsEntered, Dynamic, Equals, ExprWithPath, FormComponent, FormComponentId, FormComponentValidator, FormCtx, FormStartDateExprValue, HideZeroDecimals, Horizontal, IfElse, IncludeIf, IndexOf, IndexOfDataRetrieveCtx, InformationMessage, Instruction, IsTrue, LeafExpr, LinkCtx, LookupColumn, Mandatory, Not, NumberedList, Offset, OptionData, OptionDataValue, Page, PageId, PostcodeLookup, Radio, Section, ShortText, StandardInfo, SummariseGroupAsGrid, TemplatePath, Text, TextArea, TextWithRestrictions, TypeAhead, ValidIf, Value, Vertical }
import uk.gov.hmrc.gform.sharedmodel._

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
                      ValueParser.validateWithParser(expression, ValueParser.expr).toOption.toSeq.toList
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

  "validateBetweenFunReferenceConstraints" should {

    "validate references in daysBetween/weeksBetween function" in {

      val table = Table(
        ("expression", "expectedResult"),
        ("${daysBetween(startDate, endDate)}", Valid),
        ("${daysBetween(startDate, endDate).sum}", Valid),
        ("${weeksBetween(startDate, endDate)}", Valid),
        ("${weeksBetween(startDate, endDate).sum}", Valid),
        (
          "${daysBetween(startDate, name)}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in daysBetween/weeksBetween function should be date type"
          )
        ),
        (
          "${daysBetween(startDate, name).sum}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in daysBetween/weeksBetween function should be date type"
          )
        ),
        (
          "${weeksBetween(startDate, name)}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in daysBetween/weeksBetween function should be date type"
          )
        ),
        (
          "${weeksBetween(startDate, name).sum}",
          Invalid(
            "sections.fields.[id=infoField].infoText: Form component 'name' used in daysBetween/weeksBetween function should be date type"
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
                      ValueParser.validateWithParser(expression, ValueParser.expr).toOption.toSeq.toList
                    )
                  ),
                  true
                )
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateBetweenFunReferenceConstraints(formTemplate, allExpressions)
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
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                getChoiceComponentWithStringBasedValues("foo", true),
                mkFormComponent(
                  "page2Comp1",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(BulletedList(FormComponentId("dutyType"))))
                  ),
                  false
                )
              ),
              pageId = Some(PageId("page1"))
            )
          ),
          Valid
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                getChoiceComponentWithStringBasedValues("foo", false),
                mkFormComponent(
                  "info",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(NumberedList(FormComponentId("dutyType"))))
                  ),
                  false
                )
              ),
              pageId = Some(PageId("page1"))
            )
          ),
          Invalid(
            "sections.fields.[id=info].infoText: dutyType is not AddToList ID or a checkbox component (choice) ID"
          )
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                mkFormComponent(
                  "shortText",
                  Text(ShortText(1, 10), Value),
                  false
                ),
                mkFormComponent(
                  "info",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(DisplayAsEntered(FormComponentId("shortText"))))
                  ),
                  false
                )
              ),
              pageId = Some(PageId("page1"))
            )
          ),
          Invalid(
            "sections.fields.[id=info].infoText: shortText is not a Multiline Text field (Text Area) id in the form"
          )
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                mkFormComponent(
                  "textArea",
                  TextArea(TextWithRestrictions(0, 1000), Value, dataThreshold = None),
                  false
                ),
                mkFormComponent(
                  "info",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(DisplayAsEntered(FormComponentId("textArea"))))
                  ),
                  false
                )
              ),
              pageId = Some(PageId("page1"))
            )
          ),
          Valid
        )
      )
      forAll(table) { (sections, expected) =>
        val formTemplate = mkFormTemplate(sections)
        val allExprs: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)
        FormTemplateValidator.validateInvalidReferences(
          mkFormTemplate(sections),
          allExprs,
          List.empty[ExpressionId]
        ) shouldBe expected
      }
    }
  }

  "validate - for lookup column check" when {
    "expression refers to existing Form field" should {
      "return Valid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(LookupColumn(FormComponentId("someExistingId"), "column"), sections)
        result shouldBe Valid
      }
    }

    "expression refers to non-existing Form field" should {
      "return Invalid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(LookupColumn(FormComponentId("someNonExistingId"), "column"), sections)
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

  "validate - for HideZeroDecimals" when {
    "expression refers to existing Form field" should {
      "return Valid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result =
          FormTemplateValidator.validate(HideZeroDecimals(FormCtx(FormComponentId("someExistingId"))), sections)
        result shouldBe Valid
      }
    }

    "expression refers to non-existing Form field" should {
      "return Invalid" in {
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(mkFormComponent(id = "someExistingId"))))
        val result = FormTemplateValidator
          .validate(HideZeroDecimals(FormCtx(FormComponentId("someNonExistingId"))), sections)
        result shouldBe Invalid("Form field 'someNonExistingId' is not defined in form template.")
      }
    }
  }

  "validate postcode lookup" should {
    "return a label (or shortName) is required for postcodeLookup to use on summarySection error when a label is empty" in {
      val emptyLabel = toSmartString("")
      val postcodeLookup = mkFormComponent("postcodeLookup1", PostcodeLookup(None, None, None), emptyLabel)
      val pages = List(
        mkSectionNonRepeatingPage(
          name = "section1",
          formComponents = List(postcodeLookup)
        ).page
      )

      val result = FormTemplateValidator.validatePostcodeLookup(pages)

      result shouldBe Invalid(
        "A label (or shortName) is required for postcodeLookup 'postcodeLookup1' to use on summarySection."
      )
    }

    "succeed when shortName label is non-empty" in {
      val emptyLabel = toSmartString("")
      val shortName = toSmartString("en-shortName")
      val postcodeLookup = mkFormComponent("postcodeLookup1", PostcodeLookup(None, None, None), emptyLabel)
        .copy(shortName = Some(shortName))
      val pages = List(
        mkSectionNonRepeatingPage(
          name = "section1",
          formComponents = List(postcodeLookup)
        ).page
      )

      val result = FormTemplateValidator.validatePostcodeLookup(pages)

      result shouldBe Valid
    }

    "succeed when only welsh label is empty" in {
      val emptyWelshLabel = toSmartString("en-label", "")
      val postcodeLookup =
        mkFormComponent("postcodeLookup1", PostcodeLookup(None, None, None), emptyWelshLabel)
      val pages = List(
        mkSectionNonRepeatingPage(
          name = "section1",
          formComponents = List(postcodeLookup)
        ).page
      )

      val result = FormTemplateValidator.validatePostcodeLookup(pages)

      result shouldBe Valid
    }
  }

  "validate choice options" when {
    "String base value includes spaces" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues("f o o"))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid(
          "Choice component options non-expr 'value' must only contain letters, numbers and underscores: dutyType."
        )
      }
    }

    "String base value incudes commas" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues("f,o,o"))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid(
          "Choice component options non-expr 'value' must only contain letters, numbers and underscores: dutyType."
        )
      }
    }

    "String base value incudes empty string" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues(""))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid(
          "Choice component options cannot be empty or include only spaces: dutyType."
        )
      }
    }

    "String base value incudes spaces only" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues(" "))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid(
          "Choice component options non-expr 'value' must only contain letters, numbers and underscores: dutyType."
        )
      }
    }

    "String base value incudes no spaces" should {
      "return valid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues("foo_bar1"))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Valid
      }
    }

    "String base values are not unique" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(getChoiceComponentWithStringBasedValues("bar"))))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid("Choice component options 'value's needs to be unique: dutyType.")
      }
    }

    "Check hide choices selected is not for dynamic options based on a data retrieve" should {
      "return invalid" in {
        val sections =
          List(mkSectionNonRepeatingPage(formComponents = List(mkChoiceWithDynamicDrOptions())))
        val result = FormTemplateValidator
          .validateChoiceOptions(SectionHelper.pages(sections))
        result shouldBe Invalid(
          "'hideChoicesSelected: true' for ATL cannot be used with dynamic choice options from a data retrieve: choiceDrOptions."
        )
      }
    }
  }

  "date construct function" should {
    "validate against using 29th February" in {
      val table = Table(
        ("expression", "expectedResult"),
        ("${yearToDate('0102', endDate)}", Valid),
        (
          "${yearToDate('2902', startDate)}",
          Invalid(
            "sections.fields.[id=infoField].infoText: yearToDate can not be used with February 29th"
          )
        )
      )

      forAll(table) { (expression, expectedResult) =>
        val formTemplate = mkFormTemplate(
          List(
            mkSectionNonRepeatingPage(
              name = "section1",
              formComponents = List(
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
                      ValueParser.validateWithParser(expression, ValueParser.expr).toOption.toSeq.toList
                    )
                  ),
                  true
                )
              )
            )
          )
        )
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

        val result = FormTemplateValidator.validateDateConstructExpressions(allExpressions)
        result shouldBe expectedResult
      }
    }
  }

  "validateLabel" should {
    "validate that labels exist where required" in {
      val emptySmartString: SmartString = SmartString(LocalisedString(Map()), List())
      val table = Table(
        ("formComponent", "expectedResult"),
        (
          mkInfoMessageComponent(
            "id1",
            "Some text",
            Some(toSmartString("Summary text"))
          ),
          Invalid("info component id1 should have a non-blank label if summaryValue is specified")
        ),
        (
          mkInfoMessageComponent(
            "id1",
            "Some text",
            Some(toSmartString("Summary text")),
            toSmartString("")
          ),
          Invalid("info component id1 should have a non-blank label if summaryValue is specified")
        ),
        (
          mkInfoMessageComponent(
            "id1",
            "Some text"
          ),
          Valid
        ),
        (
          mkInfoMessageComponent(
            "id1",
            "Some text",
            Some(toSmartString("Summary text")),
            toSmartString("Some label")
          ),
          Valid
        ),
        (
          getChoiceComponentWithStringBasedValues("Test").copy(label = toSmartString("")),
          Invalid("choice component dutyType should have a non-blank label")
        ),
        (
          getChoiceComponentWithStringBasedValues("Test").copy(label = emptySmartString),
          Invalid("choice component dutyType should have a non-blank label")
        ),
        (
          getChoiceComponentWithStringBasedValues("Test"),
          Valid
        ),
        (
          mkFormComponent("startDate", Date(AnyDate, Offset(0), None), true),
          Valid
        ),
        (
          mkFormComponent("startDate", Date(AnyDate, Offset(0), None), true).copy(label = toSmartString("")),
          Invalid("date component startDate should have a non-blank label")
        ),
        (
          mkFormComponent("startDate", Date(AnyDate, Offset(0), None), true).copy(label = emptySmartString),
          Invalid("date component startDate should have a non-blank label")
        ),
        (
          mkFormComponent("text1").copy(label = emptySmartString),
          Invalid("text component text1 should have a non-blank label, unless submitMode is summaryinfoonly")
        ),
        (
          mkFormComponent("text1").copy(label = emptySmartString, onlyShowOnSummary = true),
          Valid
        ),
        (
          mkFormComponent("text1"),
          Valid
        )
      )

      forAll(table) { (formComponent, expectedResult) =>
        val sections: List[Page] = SectionHelper.pages(List[Section](mkSectionNonRepeatingPage(formComponent)))
        val result = FormTemplateValidator.validateLabel(sections)
        result shouldBe expectedResult
      }
    }
  }

  "validatePagesToRevisit" should {
    "validate that pageIdsToDisplayOnChange exist and are forward references" in {
      val comp1 = mkFormComponent("comp1", Text(ShortText.default, Value), editable = true)
      val page1 = mkSectionNonRepeatingPage("Page 1", List(comp1), pageId = Some(PageId("page1id")))

      val comp3 = mkFormComponent("comp3", Text(ShortText.default, Value), editable = true)
      val page3 = mkSectionNonRepeatingPage("Page 3", List(comp3), pageId = Some(PageId("page3id")))

      val comp4 = mkFormComponent("comp4", Text(ShortText.default, Value), editable = true)
      val page4 = mkSectionNonRepeatingPage("Page 4", List(comp4), pageId = Some(PageId("page4id")))

      val table = Table(
        ("page2Component", "expectedResult"),
        (
          mkFormComponent("comp2", Text(ShortText.default, Value), pagesToRevisit = List(PageId("pageNotFound"))),
          Invalid("Page with ID 'pageNotFound' not found in form template")
        ),
        (
          mkFormComponent("comp2", Text(ShortText.default, Value), pagesToRevisit = List(PageId("page1id"))),
          Invalid("Cannot revisit 'page1id' as it's an earlier page in form")
        ),
        (
          mkFormComponent("comp2", Text(ShortText.default, Value), pagesToRevisit = List(PageId("page2id"))),
          Invalid("Cannot revisit 'page2id' as self")
        ),
        (
          mkFormComponent("comp2", Text(ShortText.default, Value), pagesToRevisit = List(PageId("page3id"))),
          Valid
        ),
        (
          mkFormComponent(
            "comp2",
            Text(ShortText.default, Value),
            pagesToRevisit = List(PageId("page3id"), PageId("page4id"))
          ),
          Valid
        )
      )

      forAll(table) { (page2Component, expectedResult) =>
        val page2: Section.NonRepeatingPage =
          mkSectionNonRepeatingPage("Page 2", List(page2Component), pageId = Some(PageId("page2id")))
        val sections: List[Section] = List(page1, page2, page3, page4)
        val result: ValidationResult = FormTemplateValidator.validatePagesToRevisit(sections)
        result shouldBe expectedResult
      }
    }
  }

  "validateAddToListAddAnotherQuestion" should {
    "validate ATL add another question choices are valid" in {

      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkAddToList(
              name = "page1",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Valid
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              ),
              addAnotherQuestion = mkFormComponent(
                "addAnother",
                Choice(
                  Radio,
                  NonEmptyList
                    .of(toSmartString("No"), toSmartString("Yes"))
                    .map(OptionData.IndexBased(_, None, None, None, None)),
                  Vertical,
                  Nil,
                  None,
                  None,
                  None,
                  LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
                  None,
                  None,
                  false
                ),
                false
              )
            )
          ),
          Invalid("AddToList 'atlPage' addAnotherQuestion must only contain choices of 'Yes' and 'No' in that order.")
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              ),
              addAnotherQuestion = mkFormComponent(
                "addAnother",
                Choice(
                  Radio,
                  NonEmptyList
                    .of(toSmartString("Yes please"), toSmartString("No thanks"))
                    .map(OptionData.IndexBased(_, None, None, None, None)),
                  Vertical,
                  Nil,
                  None,
                  None,
                  None,
                  LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
                  None,
                  None,
                  false
                ),
                false
              )
            )
          ),
          Invalid("AddToList 'atlPage' addAnotherQuestion must only contain choices of 'Yes' and 'No' in that order.")
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              ),
              addAnotherQuestion = mkFormComponent(
                "addAnother",
                InformationMessage(
                  StandardInfo,
                  SmartString(toLocalisedString("foo"), List())
                ),
                false
              )
            )
          ),
          Invalid("AddToList 'atlPage' addAnotherQuestion must be a Choice component.")
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              ),
              addAnotherQuestion = mkFormComponent(
                "addAnother",
                Choice(
                  Radio,
                  yesNoLocalisedStrings,
                  Horizontal,
                  Nil,
                  None,
                  None,
                  None,
                  LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
                  None,
                  None,
                  false
                ),
                false
              )
            )
          ),
          Valid
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              ),
              addAnotherQuestion = mkFormComponent(
                "addAnother",
                Choice(
                  Radio,
                  yesNoLocalisedStrings,
                  Vertical,
                  Nil,
                  None,
                  None,
                  None,
                  LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
                  None,
                  None,
                  false
                ),
                false
              )
            )
          ),
          Valid
        )
      )
      forAll(table) { (sections, expected) =>
        val formTemplate = mkFormTemplate(sections)
        FormTemplateValidator.validateAddToListAddAnotherQuestion(formTemplate) shouldBe expected
      }
    }
  }

  "validateAddToListRepeatConfig" should {
    "validate that repeatsUntil and repeatsWhile are not based on a choice component with dyanamic options from a data retrieve" in {
      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkAddToList(
              name = "page1",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List.empty,
                  pageId = Some(PageId("page2"))
                ).page
              )
            )
          ),
          Valid
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List(mkChoiceWithDynamicDrOptions()),
                  pageId = Some(PageId("page2"))
                ).page
              ),
              repeatsUntil = Option(
                IncludeIf(
                  Equals(
                    ChoicesSelected(FormComponentId("choiceDrOptions")),
                    ChoicesAvailable(FormComponentId("choiceDrOptions"), Some(false))
                  )
                )
              )
            )
          ),
          Invalid(
            "AddToList 'atlPage' repeatsUntil cannot be determined by a choice component with dynamic options from a data retrieve."
          )
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.one(
                mkSectionNonRepeatingPage(
                  name = "page2",
                  formComponents = List(mkChoiceWithDynamicDrOptions()),
                  pageId = Some(PageId("page2"))
                ).page
              ),
              repeatsWhile = Option(
                IncludeIf(
                  Not(
                    Equals(
                      ChoicesSelected(FormComponentId("choiceDrOptions")),
                      ChoicesAvailable(FormComponentId("choiceDrOptions"), Some(false))
                    )
                  )
                )
              )
            )
          ),
          Invalid(
            "AddToList 'atlPage' repeatsWhile cannot be determined by a choice component with dynamic options from a data retrieve."
          )
        )
      )
      forAll(table) { (sections, expected) =>
        val pages: List[Page] = SectionHelper.pages(sections)
        val formTemplate = mkFormTemplate(sections)
        FormTemplateValidator.validateAddToListRepeatConfig(formTemplate, pages) shouldBe expected
      }
    }
  }

  "validateDataRetrieveForwardReferences" should {
    "validate that dataRetrieves do not forward reference other form components or dataRetrieves" in {
      val dr: Option[DataRetrieve] = mkHmrcTaxRatesDataRetrieve("forwardDataRetrieve")
      val fc: FormComponent = mkFormComponent("forwardFormComponent", Date(AnyDate, Offset(0), None), true)
      val fl: FormComponent = mkFormComponent("forwardLookup")
      val page2: Section.NonRepeatingPage = mkSectionNonRepeatingPage(
        name = "Page 2",
        formComponents = List(fc, fl),
        instruction = None,
        pageId = None,
        dataRetrieve = dr
      )

      val table = Table(
        ("formComponent", "dataRetrieve", "expectedResult"),
        (
          mkFormComponent("date", Date(AnyDate, Offset(0), None), true),
          mkHmrcTaxRatesDataRetrieve("dr", None, Some("${date}")),
          Valid
        ),
        (
          mkFormComponent("date", Date(AnyDate, Offset(0), None), true),
          mkHmrcTaxRatesDataRetrieve("dr", None, Some("${forwardFormComponent}")),
          Invalid("Data retrieve with id 'dr' contains forward references to [forwardFormComponent]")
        ),
        (
          mkFormComponent("date", Date(AnyDate, Offset(0), None), true),
          mkHmrcTaxRatesDataRetrieve("dr", None, Some("${dataRetrieve.forwardDataRetrieve.startDate - 1d}")),
          Invalid("Data retrieve with id 'dr' contains forward references to [forwardDataRetrieve]")
        ),
        (
          mkFormComponent("someLookup"),
          mkHmrcTaxRatesDataRetrieve("dr", Some("${someLookup.column.Code}"), None),
          Valid
        ),
        (
          mkFormComponent("someLookup", Date(AnyDate, Offset(0), None), true),
          mkHmrcTaxRatesDataRetrieve("dr", Some("${forwardLookup.column.Code}"), None),
          Invalid("Data retrieve with id 'dr' contains forward references to [forwardLookup]")
        ),
        (
          mkFormComponent("someLookup", Date(AnyDate, Offset(0), None), true),
          mkHmrcTaxRatesDataRetrieve(
            "dr",
            Some("${forwardLookup.column.Code}"),
            Some("${dataRetrieve.forwardDataRetrieve.startDate - 1d}")
          ),
          Invalid("Data retrieve with id 'dr' contains forward references to [forwardLookup,forwardDataRetrieve]")
        )
      )

      forAll(table) { (formComponent, dataRetrieve, expectedResult) =>
        val page1 = mkSectionNonRepeatingPage(
          name = "Page 1",
          formComponents = List(formComponent),
          instruction = None,
          pageId = None,
          dataRetrieve = dataRetrieve
        )

        val sections: List[Section] = List(page1, page2)
        val result: ValidationResult = FormTemplateValidator.validateDataRetrieveForwardReferences(sections)
        result shouldBe expectedResult
      }
    }
  }

  "validateNoPIITitleConstraints" should {
    "validate that no PII fields are used in titles" in {
      val pageWithPiiField = mkSectionNonRepeatingPage(
        name = "page1",
        formComponents = List(mkFormComponent("name", Text(ShortText.default, Value), editable = true)),
        pageId = Some(PageId("page1"))
      ).page
      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.of(
                pageWithPiiField,
                Page(
                  SmartString(
                    LocalisedString(Map(LangADT.En -> "{0}")),
                    List(FormCtx(FormComponentId("name")))
                  ),
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  List.empty,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  None
                )
              )
            )
          ),
          Invalid(
            "Field id [name] can only be used in title if it does not contain PII or noPIITitle is defined"
          )
        ),
        (
          List(
            mkAddToList(
              name = "atlPage",
              pages = NonEmptyList.of(
                pageWithPiiField
              )
            ),
            Section.NonRepeatingPage(
              Page(
                SmartString(
                  LocalisedString(Map(LangADT.En -> "{0}")),
                  List(IndexOf(FormComponentId("name"), 1))
                ),
                None,
                None,
                None,
                None,
                None,
                None,
                List.empty,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
              )
            )
          ),
          Invalid(
            "Field id [name] can only be used in title if it does not contain PII or noPIITitle is defined"
          )
        )
      )
      forAll(table) { (sections, expected) =>
        val formTemplate = mkFormTemplate(sections)
        val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)
        FormTemplateValidator.validateNoPIITitleConstraints(formTemplate, allExpressions) shouldBe expected
      }
    }
  }

  "validateDateFunctionReferenceConstraints" should {
    "validate and report non-date references from date functions" in {
      val table = Table(
        ("sections", "expected"),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                mkFormComponent(
                  "infoComp",
                  InformationMessage(
                    StandardInfo,
                    SmartString(
                      toLocalisedString("{0}"),
                      List(DateFunction(DateProjection.TaxYear(DateValueExpr(FormStartDateExprValue))))
                    )
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
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                mkFormComponent(
                  "notADateComponent",
                  InformationMessage(
                    StandardInfo,
                    SmartString(toLocalisedString("{0}"), List(LinkCtx(PageLink(PageId("page1")))))
                  ),
                  false
                )
              )
            ),
            mkSectionNonRepeatingPage(
              name = "page2",
              formComponents = List(
                mkFormComponent(
                  "infoComp",
                  InformationMessage(
                    StandardInfo,
                    SmartString(
                      toLocalisedString("{0}"),
                      List(
                        DateFunction(
                          DateProjection.TaxYear(DateFormCtxVar(FormCtx(FormComponentId("notADateComponent"))))
                        )
                      )
                    )
                  ),
                  false
                )
              )
            )
          ),
          Invalid(": Form component 'notADateComponent' used in taxYear function should be date type")
        ),
        (
          List(
            mkSectionNonRepeatingPage(
              name = "page1",
              formComponents = List(
                mkFormComponent(
                  "dateComp",
                  Date(
                    AnyDate,
                    Offset(0),
                    None
                  ),
                  false
                )
              )
            ),
            mkSectionNonRepeatingPage(
              name = "page2",
              formComponents = List(
                mkFormComponent(
                  "infoComp",
                  InformationMessage(
                    StandardInfo,
                    SmartString(
                      toLocalisedString("{0}"),
                      List(DateFunction(DateProjection.TaxYear(DateFormCtxVar(FormCtx(FormComponentId("dateComp"))))))
                    )
                  ),
                  false
                )
              )
            )
          ),
          Valid
        )
      )
      forAll(table) { (sections, expected) =>
        val formTemplate = mkFormTemplate(sections)
        val allExprs: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)
        FormTemplateValidator.validateDateFunctionReferenceConstraints(
          formTemplate,
          allExprs
        ) shouldBe expected
      }
    }
  }

  "validateTypeAheadChoices" should {
    "validate TypeAhead choice constraints" in {
      val table = Table(
        ("choice", "expectedResult"),
        // Valid TypeAhead with ValueBased options only
        (
          Choice(
            TypeAhead,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                None
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Valid
        ),
        // Invalid TypeAhead with IndexBased options
        (
          Choice(
            TypeAhead,
            NonEmptyList.of(
              OptionData.IndexBased(toSmartString("Option 1"), None, None, None, None)
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Invalid(
            "TypeAhead choice validation failed: dutyType. TypeAhead choices must use ValueBased options only"
          )
        ),
        // Invalid TypeAhead with Dynamic property
        (
          Choice(
            TypeAhead,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                Some(Dynamic.ATLBased(FormComponentId("someId"))),
                OptionDataValue.StringBased("value1"),
                None,
                None
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Invalid(
            "TypeAhead choice validation failed: dutyType. TypeAhead choices cannot use dynamic options"
          )
        ),
        // Valid non-TypeAhead choice (should pass validation)
        (
          Choice(
            Radio,
            NonEmptyList.of(
              OptionData.IndexBased(toSmartString("Option 1"), None, None, None, None)
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Valid
        )
      )

      forAll(table) { (choice, expectedResult) =>
        val formComponent = FormComponent(
          FormComponentId("dutyType"),
          choice,
          toSmartString("Select option"),
          false,
          None,
          None,
          None,
          validIf = None,
          mandatory = Mandatory.True,
          editable = true,
          submissible = true,
          derived = false,
          onlyShowOnSummary = false,
          None,
          None
        )
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(formComponent)))
        val pages = SectionHelper.pages(sections)
        val result = FormTemplateValidator.validateTypeAheadChoices(pages)
        result shouldBe expectedResult
      }
    }
  }

  "validateTypeAheadKeyWordUsage" should {
    "validate keyWord usage constraints" in {
      val table = Table(
        ("choice", "expectedResult"),
        // Valid TypeAhead with keyWord
        (
          Choice(
            TypeAhead,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                Some("keyword1")
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Valid
        ),
        // Valid TypeAhead without keyWord
        (
          Choice(
            TypeAhead,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                None
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Valid
        ),
        // Invalid non-TypeAhead with keyWord
        (
          Choice(
            Radio,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                Some("keyword1")
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Invalid(
            "keyWord property can only be used with TypeAhead choices: dutyType. keyWord property can only be used with TypeAhead choices. Found in options: 'Option 1' (keyWord: keyword1)"
          )
        ),
        // Invalid Checkbox with keyWord
        (
          Choice(
            Checkbox,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                Some("keyword1")
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Invalid(
            "keyWord property can only be used with TypeAhead choices: dutyType. keyWord property can only be used with TypeAhead choices. Found in options: 'Option 1' (keyWord: keyword1)"
          )
        ),
        // Valid non-TypeAhead without keyWord
        (
          Choice(
            Radio,
            NonEmptyList.of(
              OptionData.ValueBased(
                toSmartString("Option 1"),
                None,
                None,
                None,
                OptionDataValue.StringBased("value1"),
                None,
                None
              )
            ),
            Vertical,
            List.empty,
            None,
            None,
            None,
            LocalisedString(Map()),
            None,
            None,
            false
          ),
          Valid
        )
      )

      forAll(table) { (choice, expectedResult) =>
        val formComponent = FormComponent(
          FormComponentId("dutyType"),
          choice,
          toSmartString("Select option"),
          false,
          None,
          None,
          None,
          validIf = None,
          mandatory = Mandatory.True,
          editable = true,
          submissible = true,
          derived = false,
          onlyShowOnSummary = false,
          None,
          None
        )
        val sections = List(mkSectionNonRepeatingPage(formComponents = List(formComponent)))
        val pages = SectionHelper.pages(sections)
        val result = FormTemplateValidator.validateTypeAheadKeyWordUsage(pages)
        result shouldBe expectedResult
      }
    }
  }

  def mkHmrcTaxRatesDataRetrieve(
    id: String,
    codeOpt: Option[String] = None,
    dateOpt: Option[String] = None
  ): Option[DataRetrieve] = {
    val code = codeOpt.getOrElse("'FRS20'")
    val date = dateOpt.getOrElse("${TODAY}")
    val dataRetrieve =
      s"""
         |{
         |  "type": "hmrcTaxRates",
         |  "id": "$id",
         |  "parameters": {
         |    "regime": "$${'VATFRS'}",
         |    "code": "$code",
         |    "date": "$date"
         |  }
         |}
         |""".stripMargin

    val dr: Opt[DataRetrieve] = DataRetrieveDefinitions.read(Json.parse(dataRetrieve))
    dr.fold(e => JsError(e.error), r => JsSuccess(r)).asOpt
  }

  private def getChoiceComponentWithStringBasedValues(stringValue: String, isCheckbox: Boolean = false): FormComponent =
    FormComponent(
      FormComponentId("dutyType"),
      Choice(
        if (isCheckbox) Checkbox else Radio,
        NonEmptyList.of(
          OptionData
            .ValueBased(
              toSmartString("Yes", "Iawn"),
              None,
              None,
              None,
              OptionDataValue.StringBased(stringValue),
              None,
              None
            ),
          OptionData
            .ValueBased(toSmartString("No", "Na"), None, None, None, OptionDataValue.StringBased("bar"), None, None)
        ),
        if (isCheckbox) Vertical else Horizontal,
        List.empty[Int],
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None,
        false
      ),
      toSmartString("Select the tax type"),
      false,
      None,
      None,
      None,
      validIf = None,
      mandatory = Mandatory.True,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      Some(List(SummariseGroupAsGrid))
    )

  private def mkChoiceWithDynamicDrOptions() =
    FormComponent(
      FormComponentId("choiceDrOptions"),
      Choice(
        Radio,
        mkChoiceOptionsDynamicDr(),
        Vertical,
        List.empty[Int],
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None,
        true
      ),
      toSmartString("Nothing"),
      false,
      None,
      None,
      None,
      validIf = None,
      mandatory = Mandatory.True,
      editable = true,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      None,
      Some(List(SummariseGroupAsGrid))
    )

  private def mkChoiceOptionsDynamicDr() =
    NonEmptyList.of(
      OptionData.ValueBased(
        label = toSmartString("label"),
        hint = None,
        includeIf = None,
        value = OptionDataValue.StringBased("EMP"),
        dynamic = Option(
          Dynamic.DataRetrieveBased(
            IndexOfDataRetrieveCtx(
              DataRetrieveCtx(DataRetrieveId("dataRetrieveId"), DataRetrieve.Attribute("employerName")),
              0
            )
          )
        ),
        summaryValue = None,
        keyWord = None
      )
    )
}
