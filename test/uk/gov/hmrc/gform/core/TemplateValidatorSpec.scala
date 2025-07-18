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

package uk.gov.hmrc.gform.core

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.SeissEligible
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SectionGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }

class TemplateValidatorSpec extends Spec {
  private def setAllFieldIds(page: Page, id: FormComponentId): Page =
    page.copy(fields = page.fields.map(_.copy(id = id)))

  private def setAllFieldIds(sections: NonEmptyList[Section], id: FormComponentId): NonEmptyList[Section] =
    sections.map {
      case s: Section.NonRepeatingPage => s.copy(page = setAllFieldIds(s.page, id))
      case s: Section.RepeatingPage    => s.copy(page = setAllFieldIds(s.page, id))
      case s: Section.AddToList        => s.copy(pages = s.pages.map(setAllFieldIds(_, id)))
    }

  "Section.validate" should "validate unique FieldIds" in {
    import ScalaCheckDrivenPropertyChecks._
    forAll(
      oneOrMoreGen(sectionGen),
      oneOrMoreGen(sectionGen),
      formComponentIdGen("a"),
      oneOrMoreGen(sectionGen),
      oneOrMoreGen(sectionGen),
      formComponentIdGen("b")
    ) { (s11, s12, id1, s21, s22, id2) =>
      val ds1 = setAllFieldIds(s11 ::: s12, id1)
      val ds2 = setAllFieldIds(s21 ::: s22, id2)

      val allSections = ds1 ::: ds2

      val result = FormTemplateValidator.validateUniqueFields(allSections.toList, List.empty)
      result should be(Invalid(FormTemplateValidator.someFieldsAreDefinedMoreThanOnce(Set(id1, id2))))
    }
  }

  "validateUniqueFields" should "all return valid in table" in {
    import TemplateValidatorSpec._

    val groupOfGroups = List(mkGroupFormComponent("field1"), mkGroupFormComponent("field2"))

    val table =
      Table(
        ("actual", "expected"),
        (
          validateFieldIds(
            List(
              mkGroupFormComponent(
                TemplateValidatorSpec.formComponent("field1"),
                TemplateValidatorSpec.formComponent("field2")
              )
            )
          ),
          Valid
        ),
        (validateMultipleGroupIds(groupOfGroups), Valid),
        (
          validateFieldIds(
            List(
              mkGroupFormComponent(TemplateValidatorSpec.formComponent("a"), TemplateValidatorSpec.formComponent("b"))
            )
          ),
          Valid
        ),
        (validateFieldIds(List(mkGroupFormComponent(TemplateValidatorSpec.formComponent("")))), Valid),
        (
          validateFieldIds(
            List(TemplateValidatorSpec.formComponent("field1"), TemplateValidatorSpec.formComponent("field2"))
          ),
          Valid
        )
      )
    table.forEvery { case (expected, result) => expected shouldBe result }
  }

  it should "all return invalid in table" in {
    import TemplateValidatorSpec._

    val fieldId = TemplateValidatorSpec.formComponent("fieldId")
    val groupOfGroupsDuplicateIds =
      List(mkGroupFormComponent("group1", fieldId, fieldId), mkGroupFormComponent("group2", fieldId, fieldId))
    val groupOfGroups = List(mkGroupFormComponent("fieldId"), mkGroupFormComponent("fieldId"))

    val invalid = validateFieldsErrorMsg("fieldId")

    val table =
      Table(
        ("actual", "expected"),
        (validateFieldIds(List(mkGroupFormComponent(fieldId, fieldId))), invalid),
        (validateMultipleGroupIds(groupOfGroups), invalid),
        (validateFieldIds(List(fieldId, fieldId)), invalid),
        (validateFieldIds(List(mkGroupFormComponent("fieldId", fieldId))), invalid),
        (validateMultipleGroupIds(groupOfGroupsDuplicateIds), invalid)
      )
    table.forEvery { case (expected, result) => expected shouldBe result }
  }
  "validateEnrolmentIdentifier" should
    "validates ${user.enrolledIdentifier} with HmrcSimpleModule and HmrcAgentModule but invalid with Anonymous" in {
      import AuthConfigGen._
      import FormComponentGen._
      import FormTemplateValidator._
      import ScalaCheckDrivenPropertyChecks._

      forAll(
        FormTemplateGen.formTemplateGen,
        Gen.oneOf(hmrcEnrolmentModuleGen, hmrcAgentWithEnrolmentModuleGen),
        formComponentGen(),
        pageGen
      ) { (template, authConfig, fc, page) =>
        val componentType = Text(EORI, UserCtx(UserField.EnrolledIdentifier))
        val newFormComponents: List[FormComponent] = fc.copy(`type` = componentType) :: Nil
        val newPage = page.copy(fields = newFormComponents)
        val newFormKind = FormKind.Classic(List(Section.NonRepeatingPage(newPage)))
        val newTemplate = template.copy(formKind = newFormKind).copy(authConfig = authConfig)

        val isAUserCtx = userContextComponentType(formTemplate.expandedFormComponentsInMainSections)

        whenever(
          authConfig.isInstanceOf[HmrcEnrolmentModule] || authConfig
            .isInstanceOf[HmrcAgentWithEnrolmentModule] && isAUserCtx.nonEmpty
        ) {
          validateEnrolmentIdentifier(newTemplate) should be(Valid)
        }

        whenever(isAUserCtx.isEmpty) {
          validateEnrolmentIdentifier(newTemplate) should be(Valid)
        }
      }
    }

  "TemplateValidator.validateDependencyGraph" should "detect cycle in graph" in {
    val sections =
      mkSection(
        "page 1",
        mkFormComponent("a", FormCtx(FormComponentId("b"))) :: mkFormComponent(
          "b",
          FormCtx(FormComponentId("a"))
        ) :: Nil
      ) :: Nil

    val formTemplateWithOneSection = formTemplate.copy(formKind = FormKind.Classic(sections))

    val res = FormTemplateValidator.validateDependencyGraph(formTemplateWithOneSection)
    res should be(Invalid("Graph contains cycle Some(Cycle(a, a~>b, b, b~>a, a))"))

  }

  "TemplateValidator.validateEmailParameters" should "return Valid" in {

    val formComponents = List(mkFormComponent("directorFullName", Value), mkFormComponent("directorEmail", Value))
    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateEmailParameters using fields contained in declaration section" should "return Invalid" in {

    val formComponents = List(
      mkFormComponent("fieldContainedInFormTemplate", Value)
    )

    val newEmailParameters = Some(
      NonEmptyList.of(
        EmailParameter("fullName", FormCtx(FormComponentId("declarationFullName")))
      )
    )

    val newDestinationsSection: Destinations =
      DestinationList(
        NonEmptyList.of(hmrcDms),
        ackSection,
        Some(
          DeclarationSection(
            toSmartString("Declaration"),
            None,
            None,
            None,
            None,
            Some(toSmartString("ContinueLabel")),
            List(mkFormComponent("declarationFullName", Value)),
            None
          )
        )
      )

    val newFormTemplate = mkFormTemplate(formComponents, newEmailParameters, destinations = newDestinationsSection)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(
      Invalid("The following email parameters are not fields in the form template's sections: declarationFullName")
    )

  }

  "TemplateValidator.validateEmailParameters with non-existent fields" should "return Invalid" in {

    val formComponents = List(mkFormComponent("fieldNotContainedInFormTemplate", Value))
    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(
      Invalid(
        "The following email parameters are not fields in the form template's sections: directorFullName, directorEmail"
      )
    )

  }

  "TemplateValidator.validateEmailParameters with field in acknowledgement section" should "return Invalid" in {

    val formComponent = List(mkFormComponent("fieldInAcknowledgementSections", Value))

    val newEmailParameters = Some(
      NonEmptyList.of(
        EmailParameter("fieldEmailTemplateId", FormCtx(FormComponentId("fieldInAcknowledgementSection")))
      )
    )
    val newFormTemplate =
      mkFormTemplate(formComponent, newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(
      Invalid(
        "The following email parameters are not fields in the form template's sections: fieldInAcknowledgementSection"
      )
    )

  }

  "TemplateValidator.validateEmailParameters with new params" should "return Valid" in {

    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Value))
    val newEmailParameters =
      Some(
        NonEmptyList.of(EmailParameter("templateIdVariable", FormCtx(FormComponentId("fieldContainedInFormTemplate"))))
      )

    val newFormTemplate = mkFormTemplate(formComponents, newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateEmailParameters with multiple sections" should "return Valid" in {

    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Value))
    val newSection = mkSection("example", formComponents)
    val newEmailParameters =
      Some(
        NonEmptyList.of(EmailParameter("templateIdVariable", FormCtx(FormComponentId("fieldContainedInFormTemplate"))))
      )

    val newFormTemplate =
      formTemplate.copy(formKind = FormKind.Classic(List(newSection, newSection)), emailParameters = newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates" should "be invalid with dates yyyy-02-31 and yyyy-04-31" in {

    val formComponents = List(
      mkFormComponent("fieldContainedInFormTemplate", mkDate(Year.Any, Month.Exact(2), Day.Exact(31), None)),
      mkFormComponent("fieldContainedInFormTemplate", mkDate(Year.Any, Month.Exact(4), Day.Exact(31), None))
    )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(
      Invalid(
        "java.time.DateTimeException: Invalid date 'FEBRUARY 31'. java.time.DateTimeException: Invalid date 'APRIL 31'"
      )
    )

  }

  "TemplateValidator.validateDates with multiple abstract and exact valid dates" should "all return Valid" in {

    val table =
      Table(
        ("actual", "expected"),
        (dateValidation(Year.Exact(2003), Month.Any, Day.Exact(4)), Valid),
        (dateValidation(Year.Any, Month.Any, Day.Exact(15)), Valid),
        (dateValidation(Year.Any, Month.Exact(11), Day.Any), Valid),
        (dateValidation(Year.Exact(2015), Month.Exact(1), Day.Any), Valid),
        (dateValidation(Year.Exact(2015), Month.Exact(1), Day.Exact(14)), Valid),
        (dateValidation(Year.Any, Month.Any, Day.Any), Valid),
        (dateValidation(Year.Exact(2001), Month.Exact(12), Day.Exact(4)), Valid),
        (dateValidation(Year.Exact(1996), Month.Exact(11), Day.Exact(2)), Valid),
        (dateValidation(Year.Exact(2018), Month.Exact(12), Day.Exact(31)), Valid),
        (dateValidation(Year.Exact(2030), Month.Exact(4), Day.Exact(3)), Valid)
      )

    table.forEvery { case (actual, expected) => actual shouldBe expected }

  }

  "TemplateValidator.validateDates with multiple invalid dates" should "all return Invalid" in {

    val monthOutOfRangeFailure: Int => Invalid =
      month => Invalid(s"java.time.DateTimeException: Invalid value for MonthOfYear (valid values 1 - 12): $month")
    val dayOutOfRangeFailure: Int => Invalid =
      day => Invalid(s"java.time.DateTimeException: Invalid value for DayOfMonth (valid values 1 - 28/31): $day")
    val invalidDateFailure: (String, Int) => Invalid =
      (month, day) => Invalid(s"java.time.DateTimeException: Invalid date '$month $day'")

    val table = Table(
      ("actual", "expected"),
      (dateValidation(Year.Exact(2001), Month.Exact(4), Day.Exact(31)), invalidDateFailure("APRIL", 31)),
      (dateValidation(Year.Exact(2001), Month.Exact(4), Day.Exact(33)), dayOutOfRangeFailure(33)),
      (dateValidation(Year.Exact(2001), Month.Exact(13), Day.Exact(14)), monthOutOfRangeFailure(13)),
      (dateValidation(Year.Any, Month.Exact(6), Day.Exact(31)), invalidDateFailure("JUNE", 31)),
      (dateValidation(Year.Any, Month.Exact(11), Day.Exact(33)), dayOutOfRangeFailure(33))
    )

    table.forEvery { case (actual, expected) => actual shouldBe expected }

  }

  "TemplateValidator.validateDates with date 2018-02-29" should "return Invalid" in {

    val formComponents =
      List(
        mkFormComponent("fieldContainedInFormTemplate", mkDate(Year.Exact(2018), Month.Exact(2), Day.Exact(29), None))
      )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)

    res should be(Invalid("java.time.DateTimeException: Invalid date 'February 29' as '2018' is not a leap year"))

  }

  "TemplateValidator.validateDates with date 2018-02-02" should "return Valid" in {

    val formComponents =
      List(
        mkFormComponent("fieldContainedInFormTemplate", mkDate(Year.Exact(2018), Month.Exact(2), Day.Exact(2), None))
      )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates with date value 2018-02-14" should "return Valid" in {

    val formComponents =
      List(mkFormComponent("fieldContainedInFormTemplate", mkDate(Some(ExactDateValue(2018, 2, 14)))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates with date value 2018-02-31" should "return Invalid" in {

    val formComponents =
      List(mkFormComponent("fieldContainedInFormTemplate", mkDate(Some(ExactDateValue(2018, 2, 31)))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 31'"))

  }

  "TemplateValidator.validateDates with date value 2018-02-31 and date format 2018-02-14" should "return Invalid" in {

    val formComponents =
      List(
        mkFormComponent(
          "fieldContainedInFormTemplate",
          mkDate(Year.Exact(2018), Month.Exact(2), Day.Exact(14), Some(ExactDateValue(2018, 2, 31)))
        )
      )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 31'"))

  }

  "TemplateValidator.validateDates with date value 2018-02-31 and date format 2018-04-31" should "return Invalid" in {

    val formComponents =
      List(
        mkFormComponent(
          "fieldContainedInFormTemplate",
          mkDate(Year.Exact(2018), Month.Exact(4), Day.Exact(31), Some(ExactDateValue(2018, 2, 31)))
        )
      )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(
      Invalid(
        "java.time.DateTimeException: Invalid date 'APRIL 31'. java.time.DateTimeException: Invalid date 'FEBRUARY 31'"
      )
    )

  }

  "TemplateValidator.validateDates with date value 2018-02-31 in a group" should "return Invalid" in {

    val dateFormComponent = mkFormComponent("fieldContainedInFormTemplate", mkDate(Some(ExactDateValue(2018, 2, 31))))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 31'"))

  }

  "TemplateValidator.validateDates with dates 2018-02-15 and 2019-03-14 in a group" should "return Valid" in {

    val dateFormComponent1 = mkFormComponent("fieldInGroup1", mkDate(Some(ExactDateValue(2018, 2, 15))))
    val dateFormComponent2 = mkFormComponent("fieldInGroup2", mkDate(Some(ExactDateValue(2019, 3, 14))))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent1, dateFormComponent2))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates with dates 2018-02-29 and 2019-11-31 in a group" should "return Invalid" in {

    val dateFormComponent1 =
      mkFormComponent("fieldInGroup1", mkDate(Year.Exact(2018), Month.Exact(2), Day.Exact(29), None))
    val dateFormComponent2 =
      mkFormComponent("fieldInGroup2", mkDate(Year.Exact(2019), Month.Exact(11), Day.Exact(31), None))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent1, dateFormComponent2))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(
      Invalid(
        "java.time.DateTimeException: Invalid date 'February 29' as '2018' is not a leap year. java.time.DateTimeException: Invalid date 'NOVEMBER 31'"
      )
    )

  }

  "TemplateValidator.validateDates with date value 2018-02-25 in a group" should "Valid" in {

    val dateFormComponent = mkFormComponent("fieldContainedInFormTemplate", mkDate(Some(ExactDateValue(2018, 2, 25))))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates with date 2018-02-25 in a group" should "Valid" in {

    val dateFormComponent = mkFormComponent("fieldContainedInFormTemplate", mkDate(Some(ExactDateValue(2018, 2, 25))))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates with date 2018-02-30 -1" should "return Invalid" in {

    val formComponents =
      List(
        mkFormComponent(
          "fieldContainedInFormTemplate",
          mkDate(Year.Exact(2018), Month.Exact(2), Day.Exact(30), None, offsetDate = OffsetDate(-1))
        )
      )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 30'"))

  }

  "FormTemplateValidator.validateForwardReference" should "detect valid references in an In expression" in {

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)

    val table =
      Table(
        ("expr", "expected"),
        (In(FormCtx(FormComponentId("fieldA")), SeissEligible), Valid),
        (In(AuthCtx(AuthInfo.SaUtr), SeissEligible), Valid),
        (In(ParamCtx(QueryParam("test")), SeissEligible), Valid)
      )
    forAll(table) { case (booleanExpr, expected) =>
      val sectionB = baseSectionB.copy(page = baseSectionB.page.copy(includeIf = Some(IncludeIf(booleanExpr))))
      val res = FormTemplateValidator.validateForwardReference(baseSectionA :: sectionB :: Nil)
      res shouldBe expected
    }
  }

  it should "detect valid removeItemIf references in an In expression" in {

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)

    val table =
      Table(
        ("expr", "expected"),
        (In(FormCtx(FormComponentId("fieldA")), SeissEligible), Valid),
        (In(AuthCtx(AuthInfo.SaUtr), SeissEligible), Valid),
        (In(ParamCtx(QueryParam("test")), SeissEligible), Valid)
      )
    forAll(table) { case (booleanExpr, expected) =>
      val sectionB = baseSectionB.copy(page = baseSectionB.page.copy(removeItemIf = Some(RemoveItemIf(booleanExpr))))
      val res = FormTemplateValidator.validateForwardReference(baseSectionA :: sectionB :: Nil)
      res shouldBe expected
    }
  }

  it should "detect invalid references/forward references in an IncludeIf Boolean Expression" in {

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)
    val constant = Constant("c")
    val forwardRef = FormCtx(FormComponentId("fieldB"))
    val invalidRef = FormCtx(FormComponentId("a"))
    val forwardReferenceError = Invalid("id 'fieldB' named in includeIf is forward reference, which is not permitted")
    val invalidReferenceError = Invalid("id 'a' named in includeIf expression does not exist in the form")
    val table =
      Table(
        // format: off
        ("booleanExpr",                             "expected"),
        (Equals              (forwardRef, constant), forwardReferenceError),
        (GreaterThan         (forwardRef, constant), forwardReferenceError),
        (GreaterThanOrEquals (forwardRef, constant), forwardReferenceError),
        (LessThan            (forwardRef, constant), forwardReferenceError),
        (LessThanOrEquals    (forwardRef, constant), forwardReferenceError),
        (Not(Equals(forwardRef, constant)),          forwardReferenceError),
        (And(Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Or (Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Equals              (invalidRef, constant), invalidReferenceError),
        (GreaterThan         (invalidRef, constant), invalidReferenceError),
        (GreaterThanOrEquals (invalidRef, constant), invalidReferenceError),
        (LessThan            (invalidRef, constant), invalidReferenceError),
        (LessThanOrEquals    (invalidRef, constant), invalidReferenceError),
        (Not(Equals(invalidRef, constant)),          invalidReferenceError),
        (And(Equals(invalidRef, constant), IsTrue),  invalidReferenceError),
        (Or (Equals(invalidRef, constant), IsTrue),  invalidReferenceError),
        (IsTrue,  Valid),
        (IsFalse, Valid),
        (In (invalidRef, SeissEligible),  invalidReferenceError)
        // format: on
      )
    forAll(table) { case (booleanExpr, expected) =>
      val sectionA = baseSectionA.copy(page = baseSectionA.page.copy(includeIf = Some(IncludeIf(booleanExpr))))
      val res = FormTemplateValidator.validateForwardReference(sectionA :: baseSectionB :: Nil)
      res shouldBe expected
    }
  }

  it should "detect invalid references/forward references in an RemoveItemIf Boolean Expression" in {

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)
    val constant = Constant("c")
    val forwardRef = FormCtx(FormComponentId("fieldB"))
    val invalidRef = FormCtx(FormComponentId("a"))
    val forwardReferenceError =
      Invalid("id 'fieldB' named in removeItemIf is forward reference, which is not permitted")
    val invalidReferenceError = Invalid("id 'a' named in removeItemIf expression does not exist in the form")
    val table =
      Table(
        // format: off
        ("booleanExpr",                             "expected"),
        (Equals              (forwardRef, constant), forwardReferenceError),
        (GreaterThan         (forwardRef, constant), forwardReferenceError),
        (GreaterThanOrEquals (forwardRef, constant), forwardReferenceError),
        (LessThan            (forwardRef, constant), forwardReferenceError),
        (LessThanOrEquals    (forwardRef, constant), forwardReferenceError),
        (Not(Equals(forwardRef, constant)),          forwardReferenceError),
        (And(Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Or (Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Equals              (invalidRef, constant), invalidReferenceError),
        (GreaterThan         (invalidRef, constant), invalidReferenceError),
        (GreaterThanOrEquals (invalidRef, constant), invalidReferenceError),
        (LessThan            (invalidRef, constant), invalidReferenceError),
        (LessThanOrEquals    (invalidRef, constant), invalidReferenceError),
        (Not(Equals(invalidRef, constant)),          invalidReferenceError),
        (And(Equals(invalidRef, constant), IsTrue),  invalidReferenceError),
        (Or (Equals(invalidRef, constant), IsTrue),  invalidReferenceError),
        (IsTrue,  Valid),
        (IsFalse, Valid),
        (In (invalidRef, SeissEligible),  invalidReferenceError)
        // format: on
      )
    forAll(table) { case (booleanExpr, expected) =>
      val sectionA = baseSectionA.copy(page = baseSectionA.page.copy(removeItemIf = Some(RemoveItemIf(booleanExpr))))
      val res = FormTemplateValidator.validateForwardReference(sectionA :: baseSectionB :: Nil)
      res shouldBe expected
    }
  }

  it should "detect invalid references/forward references in a ValidIf Boolean expression" in {
    val constant = Constant("")
    val forwardRef = FormCtx(FormComponentId("fieldB"))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionB = mkSection("sectionB", formComponentsB)
    val invalidRef = FormCtx(FormComponentId("a"))
    val forwardReferenceError = Invalid("id 'fieldB' named in validIf is forward reference, which is not permitted")
    val invalidReferenceError = Invalid("id 'a' named in validIf expression does not exist in the form")

    val validIfInValidIf =
      (booleanExpr: BooleanExpr) => mkFormComponent("fieldA", Value).copy(validIf = Some(ValidIf(booleanExpr)))
    val validIfInValidator = (booleanExpr: BooleanExpr) =>
      mkFormComponent("fieldA", Value).copy(
        validators = FormComponentValidator(ValidIf(booleanExpr), toSmartString("")) :: Nil
      )
    val table =
      Table(
        ("booleanExpr", "mkComponent", "expected"),
        (Equals(forwardRef, constant), validIfInValidIf, forwardReferenceError),
        (Equals(invalidRef, constant), validIfInValidIf, invalidReferenceError),
        (Equals(forwardRef, constant), validIfInValidator, forwardReferenceError),
        (Equals(invalidRef, constant), validIfInValidator, invalidReferenceError)
      )
    forAll(table) { case (booleanExpr, mkComponent, expected) =>
      val formComponentA = mkComponent(booleanExpr)
      val sectionA = mkSection("sectionA", formComponentA :: Nil)
      val res = FormTemplateValidator.validateForwardReference(sectionA :: baseSectionB :: Nil)
      res shouldBe expected
    }
  }

  it should "allow reference within the same page inside of validIf" in {

    val constant = Constant("")
    val samePageRef = FormCtx(FormComponentId("fieldB"))

    val formComponents = List(
      mkFormComponent("fieldB", Value),
      mkFormComponent("fieldA", Value).copy(validIf = Some(ValidIf(Equals(samePageRef, constant))))
    )

    val section = mkSection("section", formComponents)

    val res = FormTemplateValidator.validateForwardReference(section :: Nil)
    res shouldBe Valid
  }

  it should "detect and prevent references to same page from within SmartStrings" in {
    val table = Table(
      ("sections", "expected"),
      (
        List(
          mkSectionNonRepeatingPageSmartName(
            smartName = SmartString(
              toLocalisedString("Section1 referencing ${startDate}"),
              List(FormCtx(FormComponentId("startDate")))
            ),
            formComponents = List(
              mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
              mkFormComponent("endDate", Date(AnyDate, Offset(0), None))
            )
          )
        ),
        Invalid("Cannot reference a field (startDate) on the same page")
      ),
      (
        List(
          mkSectionNonRepeatingPageSmartName(
            smartName = SmartString(
              toLocalisedString("Section1"),
              List()
            ),
            formComponents = List(
              mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
              mkFormComponentWithLabel(
                "endDate",
                Date(AnyDate, Offset(0), None),
                label = SmartString(
                  toLocalisedString("Label referencing ${startDate}"),
                  List(FormCtx(FormComponentId("startDate")))
                )
              )
            )
          )
        ),
        Invalid("Cannot reference a field (startDate) on the same page")
      ),
      (
        List(
          mkSectionNonRepeatingPageSmartName(
            smartName = SmartString(
              toLocalisedString("Section1 "),
              List()
            ),
            formComponents = List(
              mkFormComponent("startDate", Date(AnyDate, Offset(0), None)),
              mkFormComponentWithHelpText(
                "endDate",
                Date(AnyDate, Offset(0), None),
                helpText = SmartString(
                  toLocalisedString("Helptext referencing ${startDate}"),
                  List(FormCtx(FormComponentId("startDate")))
                )
              )
            )
          )
        ),
        Invalid("Cannot reference a field (startDate) on the same page")
      ),
      (
        List(
          mkSectionNonRepeatingPageSmartName(
            smartName = SmartString(
              toLocalisedString("Section1 "),
              List()
            ),
            formComponents = List(
              mkFormComponent(
                "infoMsg",
                InformationMessage(
                  StandardInfo,
                  SmartString(
                    toLocalisedString("Info message referencing ${startDate}"),
                    List(FormCtx(FormComponentId("startDate")))
                  )
                )
              ),
              mkFormComponent("startDate", Date(AnyDate, Offset(0), None))
            )
          )
        ),
        Valid
      )
    )
    forAll(table) { (sections, expected) =>
      FormTemplateValidator.validateForwardReference(sections) shouldBe expected
    }
  }

  it should "allow reference to addAnotherQuestion of AddToList section" in {

    val yesNoLocalisedStrings =
      NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))

    val addToListPage = mkSection("addToListPage", List(mkFormComponent("fieldA", Value))).page
    val addAnotherQuestion =
      mkFormComponent(
        "addAnother",
        Choice(
          YesNo,
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
        )
      )
    val referenceToAddAnotherQuestion = mkFormComponent("fieldC", Count(FormComponentId("addAnother")))

    val sectionA = mkAddToList("AddToList", NonEmptyList.one(addToListPage), addAnotherQuestion)
    val sectionB = mkSection("NonRepeated", List(referenceToAddAnotherQuestion))

    val formTemplateUpd = formTemplate.copy(formKind = FormKind.Classic(List(sectionA, sectionB)))

    val res = FormTemplateValidator.validate(List(referenceToAddAnotherQuestion.`type`), formTemplateUpd)
    res shouldBe Valid
  }

  it should "validate forward references in AddToList fields within duplicateExists boolean expressions" in {

    val yesNoLocalisedStrings =
      NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))

    val fieldA = FormCtx(FormComponentId("fieldA"))
    val fieldB = FormCtx(FormComponentId("fieldB"))
    val fieldC = FormCtx(FormComponentId("fieldC"))

    val validIfTests = Map(
      DuplicateExists(List(fieldA, fieldC)) -> Invalid(
        "id 'fieldC' named in validIf is forward reference, which is not permitted"
      ),
      DuplicateExists(List(fieldA, fieldB)) -> Valid
    )
    val addToListPage1 = mkSection("addToListPage", List(mkFormComponent("fieldA", Value))).page
    val addToListPage3 = mkSection("addToListPage", List(mkFormComponent("fieldC", Value))).page
    val addAnotherQuestion =
      mkFormComponent(
        "addAnother",
        Choice(
          YesNo,
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
        )
      )
    val outsideAddToList = mkFormComponent("fieldD", Value)

    for ((duplicateExists, validationResult) <- validIfTests) {
      val addToListPage2 =
        mkSection("addToListPage", List(mkFormComponent("fieldB", Value, Some(ValidIf(duplicateExists))))).page

      val sectionA = mkSection("NonRepeated", List(outsideAddToList))
      val sectionB =
        mkAddToList("AddToList", NonEmptyList.of(addToListPage1, addToListPage2, addToListPage3), addAnotherQuestion)

      val res = FormTemplateValidator.validateForwardReference(List(sectionA, sectionB))
      res shouldBe validationResult
    }

  }

  "TemplateValidator.validateAddToListDeclarationSection" should "disallow AddToList declarationSection if no CYA page exists" in {
    val yesNoLocalisedStrings =
      NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))
    val addToListPage = mkSection("addToListPage", List(mkFormComponent("fieldA", Value))).page
    val addAnotherQuestion =
      mkFormComponent(
        "addAnother",
        Choice(
          YesNo,
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
        )
      )
    val referenceToAddAnotherQuestion = mkFormComponent("fieldC", Count(FormComponentId("addAnother")))

    val sectionA = mkAddToList("AddToList", NonEmptyList.one(addToListPage), addAnotherQuestion, None, Some(decSection))
    val sectionB = mkSection("NonRepeated", List(referenceToAddAnotherQuestion))

    val formTemplateUpd = formTemplate.copy(formKind = FormKind.Classic(List(sectionA, sectionB)))

    val res = FormTemplateValidator.validateAddToListDeclarationSection(formTemplateUpd)
    res shouldBe Invalid("AddToList declaration section for 'addAnother' can only be defined when a CYA page exists.")
  }

  it should "allow AddToList declarationSection when CYA page does exist" in {
    val yesNoLocalisedStrings =
      NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))
    val addToListPage = mkSection("addToListPage", List(mkFormComponent("fieldA", Value))).page
    val addAnotherQuestion =
      mkFormComponent(
        "addAnother",
        Choice(
          YesNo,
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
        )
      )
    val referenceToAddAnotherQuestion = mkFormComponent("fieldC", Count(FormComponentId("addAnother")))
    val cyaPage = mkCheckYourAnswersPage("cya")
    val declarationSection = decSection.copy(fields = List(`fieldValue - info`))

    val sectionA = mkAddToList(
      "AddToList",
      NonEmptyList.one(addToListPage),
      addAnotherQuestion,
      Some(cyaPage),
      Some(declarationSection)
    )
    val sectionB = mkSection("NonRepeated", List(referenceToAddAnotherQuestion))

    val formTemplateUpd = formTemplate.copy(formKind = FormKind.Classic(List(sectionA, sectionB)))

    val res = FormTemplateValidator.validateAddToListDeclarationSection(formTemplateUpd)
    res shouldBe Valid
  }

  it should "disallow AddToList declarationSection when it contains non-readonly fields" in {
    val yesNoLocalisedStrings =
      NonEmptyList.of(toSmartString("Yes"), toSmartString("No")).map(OptionData.IndexBased(_, None, None, None, None))
    val addToListPage = mkSection("addToListPage", List(mkFormComponent("fieldA", Value))).page
    val addAnotherQuestion =
      mkFormComponent(
        "addAnother",
        Choice(
          YesNo,
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
        )
      )
    val referenceToAddAnotherQuestion = mkFormComponent("fieldC", Count(FormComponentId("addAnother")))
    val cyaPage = mkCheckYourAnswersPage("cya")
    val declarationSection = decSection.copy(fields = List(`fieldValue - number`))

    val sectionA = mkAddToList(
      "AddToList",
      NonEmptyList.one(addToListPage),
      addAnotherQuestion,
      Some(cyaPage),
      Some(declarationSection)
    )
    val sectionB = mkSection("NonRepeated", List(referenceToAddAnotherQuestion))

    val formTemplateUpd = formTemplate.copy(formKind = FormKind.Classic(List(sectionA, sectionB)))

    val res = FormTemplateValidator.validateAddToListDeclarationSection(formTemplateUpd)
    res shouldBe Invalid(
      "All fields in Declaration section for AddToList must be Info or Table or MiniSummary type. Field Id, 'number' is not an info field, table or mini summary."
    )
  }

  private def mkDate(
    year: Year,
    month: Month,
    day: Day,
    value: Option[DateValue],
    beforeAfterPrecisely: BeforeAfterPrecisely = Precisely,
    offsetDate: OffsetDate = OffsetDate(0)
  ) =
    Date(
      DateConstraints(List(DateConstraint(beforeAfterPrecisely, ConcreteDate(year, month, day), offsetDate))),
      Offset(0),
      value
    )

  private def mkDate(value: Option[DateValue]) =
    Date(
      DateConstraints(List(DateConstraint(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Any), OffsetDate(0)))),
      Offset(0),
      value
    )

  private def mkSection(name: String, formComponents: List[FormComponent]) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(name),
        None,
        None,
        None,
        None,
        None,
        None,
        formComponents,
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

  private def mkCheckYourAnswersPage(title: String) =
    CheckYourAnswersPage(
      Some(toSmartString(title)),
      None,
      toSmartString(title),
      None,
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

  private def mkAddToList(
    name: String,
    pages: NonEmptyList[Page],
    addAnotherQuestion: FormComponent,
    maybeCya: Option[CheckYourAnswersPage] = None,
    maybeDeclaration: Option[DeclarationSection] = None
  ) =
    Section.AddToList(
      toSmartString(name),
      None,
      Some(toSmartString(name)),
      AtlDescription.SmartStringBased(toSmartString(name)),
      toSmartString(name),
      toSmartString(name),
      toSmartString(name),
      None,
      pages,
      None,
      None,
      None,
      addAnotherQuestion,
      None,
      None,
      None,
      None,
      None,
      cyaPage = maybeCya,
      declarationSection = maybeDeclaration,
      displayWidth = None
    )

  private def mkFormComponent(name: String, expr: Expr, validIf: Option[ValidIf] = None) =
    FormComponent(
      FormComponentId(name),
      Text(ShortText.default, expr),
      toSmartString(name),
      false,
      None,
      None,
      None,
      validIf,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  private def mkFormComponent(name: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(name),
      ct,
      toSmartString(name),
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  private def mkFormComponentWithLabel(name: String, ct: ComponentType, label: SmartString) =
    FormComponent(
      FormComponentId(name),
      ct,
      label,
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  private def mkFormComponentWithHelpText(id: String, ct: ComponentType, helpText: SmartString) =
    FormComponent(
      FormComponentId(id),
      ct,
      toSmartString(id),
      false,
      Some(helpText),
      None,
      None,
      None,
      true,
      true,
      true,
      false,
      false,
      None,
      None,
      Nil,
      None
    )

  private def mkSectionNonRepeatingPageSmartName(
    smartName: SmartString,
    formComponents: List[FormComponent],
    instruction: Option[Instruction] = None,
    pageId: Option[PageId] = None
  ) =
    Section.NonRepeatingPage(
      Page(
        smartName,
        pageId,
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None,
        instruction,
        None,
        None,
        None,
        None,
        None,
        None
      )
    )

  private def mkFormTemplate(
    formComponents: List[FormComponent],
    emailParameters: Option[NonEmptyList[EmailParameter]] = emailParameters,
    destinations: Destinations = formTemplate.destinations
  ): FormTemplate = {
    val section = mkSection("example", formComponents)

    formTemplate
      .copy(formKind = FormKind.Classic(List(section)), emailParameters = emailParameters, destinations = destinations)
  }

  private def mkGroupFormComponent(formComponents: FormComponent*): FormComponent =
    mkFormComponent("group", Group(formComponents.toList))

  private def mkGroupFormComponent(groupName: String, formComponents: FormComponent*): FormComponent =
    mkFormComponent(groupName, Group(formComponents.toList))

  private def dateValidation(year: Year, month: Month, day: Day): ValidationResult =
    FormTemplateValidator.validateDates(
      mkFormTemplate(List(mkFormComponent("fieldContainedInFormTemplate", mkDate(year, month, day, None))))
    )

  val validateFieldsErrorMsg: String => Invalid =
    culpritName => Invalid(s"Some FieldIds are defined more than once: List($culpritName)")

  private object TemplateValidatorSpec {
    val formComponent: String => FormComponent = formId => mkFormComponent(formId, Value)

    val validateSections: List[Section] => ValidationResult = sections =>
      FormTemplateValidator.validateUniqueFields(sections, List.empty)

    val validateFieldIds: List[FormComponent] => ValidationResult = { formComponents =>
      val sections = List(mkSection("section", formComponents))
      validateSections(sections)
    }

    def validateMultipleGroupIds(formComponents: List[FormComponent]): ValidationResult = {
      val sections = List(mkSection("section", formComponents))
      validateSections(sections)
    }
  }

  implicit class FormComponentOps(fc: FormComponent) {
    def isEditable: FormComponent = fc.copy(editable = true)
    def isNonEditable: FormComponent = fc.copy(editable = false)
    def isMandatory: FormComponent = fc.copy(mandatory = true)
    def isNotMandatory: FormComponent = fc.copy(mandatory = false)
  }
}
