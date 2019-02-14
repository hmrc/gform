/*
 * Copyright 2019 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.FormTemplateValidator
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.data.NonEmptyList
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SectionGen._
import FormTemplateValidator._
import AuthConfigGen._
import FormComponentGen._

import scala.collection.immutable.List

class TemplateValidatorSpec extends Spec {
  private def setAllFieldIds(sections: NonEmptyList[Section], id: FormComponentId): NonEmptyList[Section] =
    sections.map(s => s.copy(fields = s.fields.map(_.copy(id = id))))

  "Section.validate" should "validate unique FieldIds" in {
    forAll(
      oneOrMoreGen(sectionGen),
      oneOrMoreGen(sectionGen),
      formComponentIdGen,
      oneOrMoreGen(sectionGen),
      oneOrMoreGen(sectionGen),
      formComponentIdGen
    ) { (s11, s12, id1, s21, s22, id2) =>
      val ds1 = setAllFieldIds(s11 ::: s12, id1)
      val ds2 = setAllFieldIds(s21 ::: s22, id2)

      val allSections = ds1 ::: ds2

      val result = FormTemplateValidator.validateUniqueFields(allSections.toList)
      result should be(Invalid(FormTemplateValidator.someFieldsAreDefinedMoreThanOnce(Set(id1, id2))))
    }
  }

  "validateUniqueDestinationIds" should "return an error when there are duplicate ids" in {
    import DestinationGen._
    forAll(destinationIdGen, destinationIdGen) { (id1, id2) =>
      forAll(
        oneOrMoreGen(destinationWithFixedIdGen(id1)),
        oneOrMoreGen(destinationWithFixedIdGen(id1)),
        oneOrMoreGen(destinationWithFixedIdGen(id2)),
        oneOrMoreGen(destinationWithFixedIdGen(id2)),
        destinationGen.filter(d => d.id != id1 && d.id != id2)
      ) { (d1WithId1, d2WithId1, d1WithId2, d2WithId2, uniqueD) =>
        val destinations = Destinations.DestinationList(uniqueD :: d1WithId1 ::: d2WithId1 ::: d1WithId2 ::: d2WithId2)

        FormTemplateValidator.validateUniqueDestinationIds(destinations) should be(
          Invalid(FormTemplateValidator.someDestinationIdsAreUsedMoreThanOnce(Set(id1, id2))))
      }
    }
  }

  it should "validates ${user.enrolledIdentifier} with HmrcSimpleModule and HmrcAgentModule" in {
    forAll(FormTemplateGen.formTemplateGen, Gen.oneOf(hmrcEnrolmentModuleGen, hmrcAgentWithEnrolmentModuleGen)) {
      (template, authConfig) =>
        val newTemplate = template.copy(authConfig = authConfig)
        val isAUserCtx = userContextComponentType(formTemplate.expandFormTemplate.allFCs)

        whenever(authConfig.isInstanceOf[HmrcEnrolmentModule] || authConfig.isInstanceOf[HmrcAgentWithEnrolmentModule]) {
          validateEnrolmentIdentifier(newTemplate, _ => true) should be(Valid)
        }

        whenever(!isAUserCtx) {
          validateEnrolmentIdentifier(newTemplate, userContextComponentType) should be(Valid)
        }
    }
  }

  it should "invalid with Anonymous" in {
    forAll(FormTemplateGen.formTemplateGen, anonymousGen) { (template, authConfig) =>
      val ano = template.copy(authConfig = authConfig)

      whenever(authConfig == Anonymous) {
        val result = validateEnrolmentIdentifier(ano, _ => true)
        result should be(Invalid("You used '${user.enrolledIdentifier}' but you didn't provide 'serviceId'."))
      }
    }
  }

  it should "not return an error when there are no duplicate ids" in {
    import DestinationGen._
    forAll(destinationGen, destinationGen) { (d1, d2) =>
      whenever(d1.id != d2.id) {
        FormTemplateValidator.validateUniqueDestinationIds(Destinations.DestinationList(NonEmptyList.of(d1, d2))) should be(
          Valid)
      }
    }
  }

  "validateOneOrMoreHmrcDmsDestination" should "not return an error when there is one or more hmrcDms destinations" in {
    import DestinationGen._
    forAll(PrimitiveGen.oneOrMoreGen(hmrcDmsGen)) { hmrcDmss =>
      val destinations = Destinations.DestinationList(hmrcDmss)
      FormTemplateValidator.validateOneOrMoreHmrcDmsDestination(destinations) should be(Valid)
    }
  }

  it should "return an error when there are no HmrcDms Destinations" in {
    import DestinationGen._
    forAll(handlebarsHttpApiGen) { d1 =>
      val destinations = Destinations.DestinationList(NonEmptyList.of(d1))
      FormTemplateValidator.validateOneOrMoreHmrcDmsDestination(destinations) should be(
        Invalid(FormTemplateValidator.oneOrMoreHmrcDestinationsRequired))
    }
  }

  private val businessDetailsSection = mkSection(
    "Business details",
    mkFormComponent("nameOfBusiness", Value) ::
      mkFormComponent("businessAddress", Address(international = false)) :: Nil
  )

  private val sectionWithDate = mkSection(
    "Business details",
    mkFormComponent("nameOfBusiness", Value) ::
      mkFormComponent("startDate", Date(AnyDate, Offset(0), None)) :: Nil
  )

  private val sectionWithCheckbox = mkSection(
    "Business details",
    mkFormComponent("nameOfBusiness", Value) ::
      mkFormComponent(
      "dutyType",
      Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None)) :: Nil
  )

  private val sectionWithRadio = mkSection(
    "Business details",
    mkFormComponent("nameOfBusiness", Value) ::
      mkFormComponent(
      "dutyType",
      Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None)) :: Nil
  )

  private val sectionWithYesNo = mkSection(
    "Business details",
    mkFormComponent("nameOfBusiness", Value) ::
      mkFormComponent("taxType", Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None)) :: Nil
  )

  "TemplateValidator.getMatchingSection" should "find matching section containing address component" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("businessAddress-street1"), "street1"),
      FormField(FormComponentId("businessAddress-street2"), "street2"),
      FormField(FormComponentId("businessAddress-street3"), "street3"),
      FormField(FormComponentId("businessAddress-street4"), "street4"),
      FormField(FormComponentId("businessAddress-postcode"), "postcode"),
      FormField(FormComponentId("businessAddress-country"), "country")
    )
    val sections = List(businessDetailsSection)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "succeed to find matching section containing address component when optional fields are not present" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("businessAddress-street1"), "street1"),
      FormField(FormComponentId("businessAddress-postcode"), "postcode")
    )
    val sections = List(businessDetailsSection)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing address component when mandatory fields are not present" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("businessAddress.town"), "town"),
      FormField(FormComponentId("businessAddress.county"), "county"),
      FormField(FormComponentId("businessAddress.postcode"), "postcode")
    )
    val sections = List(businessDetailsSection)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "fail to find matching section containing address component when field not in form template is present" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("businessAddress.street1"), "street1"),
      FormField(FormComponentId("businessAddress.town"), "town"),
      FormField(FormComponentId("businessAddress.county"), "county"),
      FormField(FormComponentId("businessAddress.postcode"), "postcode"),
      FormField(FormComponentId("attacker.injected.field"), "); drop all tables;")
    )
    val sections = List(businessDetailsSection)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "find matching section containing date component" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("startDate-day"), "1"),
      FormField(FormComponentId("startDate-month"), "12"),
      FormField(FormComponentId("startDate-year"), "2000")
    )
    val sections = List(sectionWithDate)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing date component when mandatory fields are not present" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("startDate.month"), "12"),
      FormField(FormComponentId("startDate.year"), "2000")
    )
    val sections = List(sectionWithDate)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "fail to find matching section containing date component when field not in form template is present" in {

    val formFields = List(
      FormField(FormComponentId("nameOfBusiness"), "Apple inc."),
      FormField(FormComponentId("startDate.day"), "1"),
      FormField(FormComponentId("startDate.month"), "12"),
      FormField(FormComponentId("startDate.year"), "2000"),
      FormField(FormComponentId("attacker.injected.field"), "); drop all tables;")
    )
    val sections = List(sectionWithDate)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "succeed to find matching section containing only text field which is not mandatory" in {

    val section = mkSection(
      "Business details",
      mkFormComponent("nameOfBusiness", Value).isNotMandatory :: Nil
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing only text field which is mandatory" in {

    val section = mkSection(
      "Business details",
      mkFormComponent("nameOfBusiness", Value).isMandatory :: Nil
    )

    val formFields = List() // Nothing submittedForm

    val sections = List(section)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "find matching section containing Checkbox component" in {

    val formFields =
      List(FormField(FormComponentId("nameOfBusiness"), "Apple inc."), FormField(FormComponentId("dutyType"), "0,1"))
    val sections = List(sectionWithCheckbox)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "find matching section containing Radio component" in {

    val formFields =
      List(FormField(FormComponentId("nameOfBusiness"), "Apple inc."), FormField(FormComponentId("dutyType"), "0"))
    val sections = List(sectionWithRadio)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "find matching section containing YesNo component" in {

    val formFields =
      List(FormField(FormComponentId("nameOfBusiness"), "Apple inc."), FormField(FormComponentId("taxType"), "0"))
    val sections = List(sectionWithYesNo)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
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
    val newSection = mkSection("example", formComponents)
    val newFormTemplate = formTemplate.copy(sections = List(newSection))

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateEmailParameters using fields contained in declaration section" should "return Valid" in {

    val formComponents = List(
      mkFormComponent("fieldContainedInFormTemplate", Value)
    )

    val newSection = mkSection("example", formComponents)
    val newEmailParameters = Some(
      NonEmptyList.of(
        EmailParameter("fullName", FormCtx("declarationFullName"))
      ))
    val newDeclarationSection =
      DeclarationSection("Declaration", None, None, List(mkFormComponent("declarationFullName", Value)))
    val newFormTemplate = formTemplate.copy(
      sections = List(newSection),
      emailParameters = newEmailParameters,
      declarationSection = newDeclarationSection)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Valid)

  }

  "TemplateValidator.validateEmailParameters with non-existent fields" should "return Invalid" in {

    val formComponents = List(mkFormComponent("fieldNotContainedInFormTemplate", Value))
    val newSection = mkSection("example", formComponents)
    val newFormTemplate = formTemplate.copy(sections = List(newSection))

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Invalid(
      "The following email parameters are not fields in the form template's sections or the declaration section: List(directorFullName, directorEmail)"))

  }

  "TemplateValidator.validateEmailParameters with field in acknowledgement section" should "return Invalid" in {

    val formComponent = List(mkFormComponent("fieldInAcknowledgementSection", Value))
    val newAcknowledgementSection =
      AcknowledgementSection("ack section with email param field", None, None, formComponent)
    val newEmailParameters = Some(
      NonEmptyList.of(
        EmailParameter("fieldEmailTemplateId", FormCtx("fieldInAcknowledgementSection"))
      ))
    val newFormTemplate =
      formTemplate.copy(acknowledgementSection = newAcknowledgementSection, emailParameters = newEmailParameters)

    val res = FormTemplateValidator.validateEmailParameter(newFormTemplate)
    res should be(Invalid(
      "The following email parameters are not fields in the form template's sections or the declaration section: List(fieldInAcknowledgementSection)"))

  }

  "TemplateValidator.validateEmailParameters with new params" should "return Valid" in {

    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Value))
    val newSection = mkSection("example", formComponents)
    val newEmailParameters =
      Some(NonEmptyList.of(EmailParameter("templateIdVariable", FormCtx("fieldContainedInFormTemplate"))))
    val newFormTemplate = formTemplate.copy(sections = List(newSection), emailParameters = newEmailParameters)

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

  "TemplateValidator.getAllFieldIdsFromFormTemplate with groups in section" should "return Valid" in {

    val groupFields = List(mkFormComponent("fieldContainedInGroup", Value))
    val formComponents = List(mkFormComponent("fieldContainedInFormTemplate", Group(groupFields, Vertical)))

    val newSection = mkSection("example", formComponents)

    val newFormTemplate = formTemplate.copy(sections = List(newSection))

    val res = FormTemplateValidator.getAllFieldIdsFromFormTemplate(newFormTemplate)
    res should be(List(FormComponentId("fieldContainedInGroup")))
  }

  private def mkSection(name: String, formComponents: List[FormComponent]) =
    Section(
      name,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      formComponents,
      None,
      None
    )

  private def mkFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(AnyText, expr),
      name,
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
      name,
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

  implicit class FormComponentOps(fc: FormComponent) {
    def isEditable: FormComponent = fc.copy(editable = true)
    def isNonEditable: FormComponent = fc.copy(editable = false)
    def isMandatory: FormComponent = fc.copy(mandatory = true)
    def isNotMandatory: FormComponent = fc.copy(mandatory = false)
  }
}
