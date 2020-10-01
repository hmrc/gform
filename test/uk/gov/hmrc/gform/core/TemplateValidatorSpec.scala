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

package uk.gov.hmrc.gform.core

import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.SeissEligible
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SectionGen._

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

      val result = FormTemplateValidator.validateUniqueFields(allSections.toList)
      result should be(Invalid(FormTemplateValidator.someFieldsAreDefinedMoreThanOnce(Set(id1, id2))))
    }
  }

  "validateUniqueFields" should "all return valid in table" in {
    import TableDrivenPropertyChecks._
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
                TemplateValidatorSpec.formComponent("field2")))),
          Valid),
        (validateMultipleGroupIds(groupOfGroups), Valid),
        (
          validateFieldIds(List(
            mkGroupFormComponent(TemplateValidatorSpec.formComponent("a"), TemplateValidatorSpec.formComponent("b")))),
          Valid),
        (validateFieldIds(List(mkGroupFormComponent(TemplateValidatorSpec.formComponent("")))), Valid),
        (
          validateFieldIds(
            List(TemplateValidatorSpec.formComponent("field1"), TemplateValidatorSpec.formComponent("field2"))),
          Valid)
      )
    table.forEvery { case (expected, result) => expected shouldBe result }
  }

  it should "all return invalid in table" in {
    import TableDrivenPropertyChecks._
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
    import FormTemplateValidator._
    import AuthConfigGen._
    import FormComponentGen._
    import ScalaCheckDrivenPropertyChecks._

    forAll(
      FormTemplateGen.formTemplateGen,
      Gen.oneOf(hmrcEnrolmentModuleGen, hmrcAgentWithEnrolmentModuleGen),
      formComponentGen(),
      pageGen) { (template, authConfig, fc, page) =>
      val componentType = Text(EORI, UserCtx(UserField.EnrolledIdentifier))
      val newFormComponents: List[FormComponent] = fc.copy(`type` = componentType) :: Nil
      val newPage = page.copy(fields = newFormComponents)
      val newSections = List(Section.NonRepeatingPage(newPage))
      val newTemplate = template.copy(sections = newSections).copy(authConfig = authConfig)

      val isAUserCtx = userContextComponentType(formTemplate.expandedFormComponentsInMainSections)

      whenever(
        authConfig.isInstanceOf[HmrcEnrolmentModule] || authConfig
          .isInstanceOf[HmrcAgentWithEnrolmentModule] && isAUserCtx.nonEmpty) {
        validateEnrolmentIdentifier(newTemplate) should be(Valid)
      }

      whenever(isAUserCtx.isEmpty) {
        validateEnrolmentIdentifier(newTemplate) should be(Valid)
      }
    }
  }

  "TemplateValidator.validateDependencyGraph" should "detect cycle in graph" in {
    val sections =
      mkSection("page 1", mkFormComponent("a", FormCtx("b")) :: mkFormComponent("b", FormCtx("a")) :: Nil) :: Nil

    val formTemplateWithOneSection = formTemplate.copy(sections = sections)

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
        EmailParameter("fullName", FormCtx("declarationFullName"))
      ))

    val newDestinationsSection: Destinations =
      DestinationList(
        NonEmptyList.of(hmrcDms),
        ackSection,
        DeclarationSection(
          toSmartString("Declaration"),
          None,
          None,
          List(mkFormComponent("declarationFullName", Value))))

    val newFormTemplate = mkFormTemplate(formComponents, newEmailParameters, destinations = newDestinationsSection)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(
      Invalid("The following email parameters are not fields in the form template's sections: declarationFullName"))

  }

  "TemplateValidator.validateEmailParameters with non-existent fields" should "return Invalid" in {

    val formComponents = List(mkFormComponent("fieldNotContainedInFormTemplate", Value))
    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Invalid(
      "The following email parameters are not fields in the form template's sections: directorFullName, directorEmail"))

  }

  "TemplateValidator.validateEmailParameters with field in acknowledgement section" should "return Invalid" in {

    val formComponent = List(mkFormComponent("fieldInAcknowledgementSections", Value))
    val newAcknowledgementSection =
      AcknowledgementSection(
        toSmartString("ack section with email param field"),
        None,
        None,
        formComponent,
        true,
        Some(
          AcknowledgementSectionPdf(
            Some(toSmartString("It's a Acknowledgement Section Pdf header.")),
            Some(toSmartString("It's a Acknowledgement Section Pdf footer."))))
      )

    val newEmailParameters = Some(
      NonEmptyList.of(
        EmailParameter("fieldEmailTemplateId", FormCtx("fieldInAcknowledgementSection"))
      ))
    val newFormTemplate =
      mkFormTemplate(formComponent, newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(
      Invalid(
        "The following email parameters are not fields in the form template's sections: fieldInAcknowledgementSection"))

  }

  "TemplateValidator.validateEmailParameters with new params" should "return Valid" in {

    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Value))
    val newEmailParameters =
      Some(NonEmptyList.of(EmailParameter("templateIdVariable", FormCtx("fieldContainedInFormTemplate"))))

    val newFormTemplate = mkFormTemplate(formComponents, newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateEmailParameters with multiple sections" should "return Valid" in {

    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Value))
    val newSection = mkSection("example", formComponents)
    val newEmailParameters =
      Some(NonEmptyList.of(EmailParameter("templateIdVariable", FormCtx("fieldContainedInFormTemplate"))))

    val newFormTemplate =
      formTemplate.copy(sections = List(newSection, newSection), emailParameters = newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateDates" should "be invalid with dates yyyy-02-31 and yyyy-04-31" in {

    val formComponents = List(
      mkFormComponent("fieldContainedInFormTemplate", mkDate(AnyYear, ExactMonth(2), ExactDay(31), None)),
      mkFormComponent("fieldContainedInFormTemplate", mkDate(AnyYear, ExactMonth(4), ExactDay(31), None))
    )

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid(
      "java.time.DateTimeException: Invalid date 'FEBRUARY 31'. java.time.DateTimeException: Invalid date 'APRIL 31'"))

  }

  "TemplateValidator.validateDates with multiple abstract and exact valid dates" should "all return Valid" in {
    import TableDrivenPropertyChecks._

    val table =
      Table(
        ("actual", "expected"),
        (dateValidation(ExactYear(2003), AnyMonth, ExactDay(4)), Valid),
        (dateValidation(AnyYear, AnyMonth, ExactDay(15)), Valid),
        (dateValidation(AnyYear, ExactMonth(11), AnyDay), Valid),
        (dateValidation(ExactYear(2015), ExactMonth(1), AnyDay), Valid),
        (dateValidation(ExactYear(2015), ExactMonth(1), ExactDay(14)), Valid),
        (dateValidation(AnyYear, AnyMonth, AnyDay), Valid),
        (dateValidation(ExactYear(2001), ExactMonth(12), ExactDay(4)), Valid),
        (dateValidation(ExactYear(1996), ExactMonth(11), ExactDay(2)), Valid),
        (dateValidation(ExactYear(2018), ExactMonth(12), ExactDay(31)), Valid),
        (dateValidation(ExactYear(2030), ExactMonth(4), ExactDay(3)), Valid)
      )

    table.forEvery { case (actual, expected) => actual shouldBe expected }

  }

  "TemplateValidator.validateDates with multiple invalid dates" should "all return Invalid" in {
    import TableDrivenPropertyChecks._

    val monthOutOfRangeFailure: Int => Invalid =
      month => Invalid(s"java.time.DateTimeException: Invalid value for MonthOfYear (valid values 1 - 12): $month")
    val dayOutOfRangeFailure: Int => Invalid =
      day => Invalid(s"java.time.DateTimeException: Invalid value for DayOfMonth (valid values 1 - 28/31): $day")
    val invalidDateFailure: (String, Int) => Invalid =
      (month, day) => Invalid(s"java.time.DateTimeException: Invalid date '$month $day'")

    val table = Table(
      ("actual", "expected"),
      (dateValidation(ExactYear(2001), ExactMonth(4), ExactDay(31)), invalidDateFailure("APRIL", 31)),
      (dateValidation(ExactYear(2001), ExactMonth(4), ExactDay(33)), dayOutOfRangeFailure(33)),
      (dateValidation(ExactYear(2001), ExactMonth(13), ExactDay(14)), monthOutOfRangeFailure(13)),
      (dateValidation(AnyYear, ExactMonth(6), ExactDay(31)), invalidDateFailure("JUNE", 31)),
      (dateValidation(AnyYear, ExactMonth(11), ExactDay(33)), dayOutOfRangeFailure(33))
    )

    table.forEvery { case (actual, expected) => actual shouldBe expected }

  }

  "TemplateValidator.validateDates with date 2018-02-29" should "return Invalid" in {

    val formComponents =
      List(mkFormComponent("fieldContainedInFormTemplate", mkDate(ExactYear(2018), ExactMonth(2), ExactDay(29), None)))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)

    res should be(Invalid("java.time.DateTimeException: Invalid date 'February 29' as '2018' is not a leap year"))

  }

  "TemplateValidator.validateDates with date 2018-02-02" should "return Valid" in {

    val formComponents =
      List(mkFormComponent("fieldContainedInFormTemplate", mkDate(ExactYear(2018), ExactMonth(2), ExactDay(2), None)))

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
          mkDate(ExactYear(2018), ExactMonth(2), ExactDay(14), Some(ExactDateValue(2018, 2, 31)))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 31'"))

  }

  "TemplateValidator.validateDates with date value 2018-02-31 and date format 2018-04-31" should "return Invalid" in {

    val formComponents =
      List(
        mkFormComponent(
          "fieldContainedInFormTemplate",
          mkDate(ExactYear(2018), ExactMonth(4), ExactDay(31), Some(ExactDateValue(2018, 2, 31)))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid(
      "java.time.DateTimeException: Invalid date 'APRIL 31'. java.time.DateTimeException: Invalid date 'FEBRUARY 31'"))

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
      mkFormComponent("fieldInGroup1", mkDate(ExactYear(2018), ExactMonth(2), ExactDay(29), None))
    val dateFormComponent2 =
      mkFormComponent("fieldInGroup2", mkDate(ExactYear(2019), ExactMonth(11), ExactDay(31), None))

    val formComponents = List(mkFormComponent("group", Group(List(dateFormComponent1, dateFormComponent2))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid(
      "java.time.DateTimeException: Invalid date 'February 29' as '2018' is not a leap year. java.time.DateTimeException: Invalid date 'NOVEMBER 31'"))

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
          mkDate(ExactYear(2018), ExactMonth(2), ExactDay(30), None, offsetDate = OffsetDate(-1))))

    val newFormTemplate = mkFormTemplate(formComponents)

    val res = FormTemplateValidator.validateDates(newFormTemplate)
    res should be(Invalid("java.time.DateTimeException: Invalid date 'FEBRUARY 30'"))

  }

  "FormTemplateValidator.validateForwardReference" should "detect valid references in an In expression" in {
    import TableDrivenPropertyChecks._

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)

    val table =
      Table(
        // format: off
        ("expr", "expected"),
        (In (FormCtx("fieldB"), SeissEligible),  Valid),
        (In (AuthCtx(SaUtr), SeissEligible),  Valid),
        (In (ParamCtx(QueryParam("test")), SeissEligible),  Valid)
        // format: on
      )
    forAll(table) {
      case (booleanExpr, expected) =>
        val sectionA = baseSectionA.copy(page = baseSectionA.page.copy(includeIf = Some(IncludeIf(booleanExpr))))
        val res = FormTemplateValidator.validateForwardReference(sectionA :: baseSectionB :: Nil)
        res shouldBe expected
    }
  }

  "FormTemplateValidator.validateForwardReference" should "detect invalid references/forward references in a Boolean Expression" in {
    import TableDrivenPropertyChecks._

    val formComponentsA = List(mkFormComponent("fieldA", Value))
    val formComponentsB = List(mkFormComponent("fieldB", Value))
    val baseSectionA = mkSection("sectionA", formComponentsA)
    val baseSectionB = mkSection("sectionB", formComponentsB)
    val constant = Constant("c")
    val forwardRef = FormCtx("fieldB")
    val invalidRef = FormCtx("a")
    val forwardReferenceError = Invalid("id 'fieldB' named in includeIf is forward reference, which is not permitted")
    val invalidReferenceError = Invalid("id 'a' named in includeIf expression does not exist in a form")
    val table =
      Table(
        // format: off
        ("booleanExpr",                             "expected"),
        (Equals              (forwardRef, constant), forwardReferenceError),
        (NotEquals           (forwardRef, constant), forwardReferenceError),
        (GreaterThan         (forwardRef, constant), forwardReferenceError),
        (GreaterThanOrEquals (forwardRef, constant), forwardReferenceError),
        (LessThan            (forwardRef, constant), forwardReferenceError),
        (LessThanOrEquals    (forwardRef, constant), forwardReferenceError),
        (Not(Equals(forwardRef, constant)),          forwardReferenceError),
        (And(Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Or (Equals(forwardRef, constant), IsTrue),  forwardReferenceError),
        (Equals              (invalidRef, constant), invalidReferenceError),
        (NotEquals           (invalidRef, constant), invalidReferenceError),
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
    forAll(table) {
      case (booleanExpr, expected) =>
        val sectionA = baseSectionA.copy(page = baseSectionA.page.copy(includeIf = Some(IncludeIf(booleanExpr))))
        val res = FormTemplateValidator.validateForwardReference(sectionA :: baseSectionB :: Nil)
        res shouldBe expected
    }
  }

  private def mkDate(
    year: Year,
    month: Month,
    day: Day,
    value: Option[DateValue],
    beforeAfterPrecisely: BeforeAfterPrecisely = Precisely,
    offsetDate: OffsetDate = OffsetDate(0)) =
    Date(
      DateConstraints(List(DateConstraint(beforeAfterPrecisely, ConcreteDate(year, month, day), offsetDate))),
      Offset(0),
      value)

  private def mkDate(value: Option[DateValue]) =
    Date(
      DateConstraints(List(DateConstraint(Precisely, ConcreteDate(AnyYear, AnyMonth, AnyDay), OffsetDate(0)))),
      Offset(0),
      value)

  private def mkSection(name: String, formComponents: List[FormComponent]) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(name),
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None
      ))

  private def mkFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(BasicText, expr),
      toSmartString(name),
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

  private def mkFormComponent(name: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(name),
      ct,
      toSmartString(name),
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

  private def mkFormTemplate(
    formComponents: List[FormComponent],
    emailParameters: Option[NonEmptyList[EmailParameter]] = emailParameters,
    destinations: Destinations = formTemplate.destinations): FormTemplate = {
    val section = mkSection("example", formComponents)

    formTemplate
      .copy(sections = List(section), emailParameters = emailParameters, destinations = destinations)
  }

  private def mkGroupFormComponent(formComponents: FormComponent*): FormComponent =
    mkFormComponent("group", Group(formComponents.toList))

  private def mkGroupFormComponent(groupName: String, formComponents: FormComponent*): FormComponent =
    mkFormComponent(groupName, Group(formComponents.toList))

  private def dateValidation(year: Year, month: Month, day: Day): ValidationResult =
    FormTemplateValidator.validateDates(
      mkFormTemplate(List(mkFormComponent("fieldContainedInFormTemplate", mkDate(year, month, day, None)))))

  val validateFieldsErrorMsg: String => Invalid =
    culpritName => Invalid(s"Some FieldIds are defined more than once: List($culpritName)")

  private object TemplateValidatorSpec {
    val formComponent: String => FormComponent = formId => mkFormComponent(formId, Value)

    val validateSections: List[Section] => ValidationResult = sections =>
      FormTemplateValidator.validateUniqueFields(sections)

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
