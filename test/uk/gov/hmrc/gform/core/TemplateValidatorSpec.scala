/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateSchema, FormTemplateValidator }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.form._

class TemplateValidatorSpec extends Spec {

  "Section.validate" should "validate unique FieldIds" in {
    val template =
      """{
        |  "_id": "IPT100",
        |  "formName": "Insurance Premium Tax Return | Yswiriant Ffurflen Dreth Premiwm",
        |  "description": "Fill in your insurance premium tax return form online | Llenwch eich ffurflen dreth premiwm yswiriant ar-lein",
        |  "characterSet": "UTF-8",
        |  "dmsSubmission": {
        |    "customerId": "nino",
        |    "classificationType": "BT-NRU-Environmental",
        |    "businessArea": "FinanceOpsCorpT"
        |  },
        |  "submitSuccessUrl": "http://www.google.co.uk",
        |  "submitErrorUrl": "http://www.yahoo.co.uk",
        |  "sections": [
        |    {
        |      "title": "Calculation 1/2 | eich calculationewq",
        |      "fields": [
        |        {
        |          "type": "choice",
        |          "id": "dutyType",
        |          "label": "Select the tax type for the duty you need to pay from the below",
        |          "choices": [
        |            "Tax type 590 - Natural gas",
        |            "Tax type 591 - Other gas"
        |          ],
        |          "value": "1"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "dutyType",
        |          "label": "Select the tax type for the duty you need to pay from the below",
        |          "choices": [
        |            "Tax type 590 - Natural gas",
        |            "Tax type 591 - Other gas"
        |          ],
        |          "value": "1"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "dutyTypes",
        |          "label": "Select the tax type for the duty you need to pay from the below",
        |          "choices": [
        |            "Tax type 595 - Natural gas",
        |            "Tax type 596 - Other gas",
        |            "Tax type 597 - Super gas",
        |            "Tax type 598 - Empty gas"
        |          ],
        |          "multivalue": "yes",
        |          "mandatory": "true",
        |          "value": "1,2,3"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "testChoice",
        |          "label": "Test mandatory=false and nothind selected",
        |          "choices": [
        |            "295 - Natural gas",
        |            "296 - SuperOther gas",
        |            "297 - SuperBNatural gas",
        |            "298 - Full throttle gas"
        |          ],
        |          "multivalue": "yes",
        |          "mandatory": "true",
        |          "value": "0,1,2"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "isPremisesAddressBusinessAddress",
        |          "label": "Is the address of the premises the same as your business address?",
        |          "format": "yesno",
        |          "value": "0"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "isPremisesAddressBusinessAddress2",
        |          "label": "Is the address of the premises the same as your business address again?",
        |          "format": "yesno",
        |          "value": "1"
        |        },
        |        {
        |          "type": "choice",
        |          "id": "isPremisesAddressBusinessAddress2",
        |          "label": "Is the address of the premises the same as your business address again?",
        |          "format": "yesno",
        |          "value": "1"
        |        },
        |        {
        |          "id": "amountA",
        |          "label": "Amount A | Rhif A",
        |          "format": "positiveNumber",
        |          "mandatory": "true"
        |        }
        |      ]
        |    },
        |    {
        |      "title": "Calculation 2/2 | eich calculationewq",
        |      "fields": [
        |        {
        |          "id": "amountB",
        |          "label": "Amount B | Rhif B",
        |          "format": "number",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "sum",
        |          "label": "Sum | Eich sumolaf",
        |          "mandatory": "true",
        |          "value": "${amountA + amountB}"
        |        }
        |      ]
        |    },
        |    {
        |      "title": "Your details | eich manylion",
        |      "fields": [
        |        {
        |          "id": "firstName",
        |          "label": "Your first name | Eich enw cyntaf",
        |          "mandatory": "true",
        |          "type": "text"
        |        },
        |        {
        |          "id": "iptRegNum",
        |          "label": "Insurance Premium Tax (IPT) registration number | Treth Premiwm Yswiriant (IPT) rhif cofrestru",
        |          "readonly": "true",
        |          "mandatory": "true",
        |          "type": "text"
        |        },
        |        {
        |          "id": "lastName",
        |          "label": "Your last name | Eich enw olaf",
        |          "mandatory": "true",
        |          "type": "text"
        |        },
        |        {
        |          "id": "address",
        |          "label": "Your Address | Eich enw cyntaf",
        |          "mandatory": "true",
        |          "type": "address"
        |        },
        |        {
        |          "id": "accPeriodStartDate",
        |          "type": "date",
        |          "label": "Accounting period start date",
        |          "helpText": "For example, 31 3 1980",
        |          "format":"after 2016-09-05 -1",
        |          "offset":"5",
        |          "mandatory": "false",
        |          "value": "next-01-15"
        |        }
        |      ]
        |    },
        |    {
        |      "title": "Business details | manylion Busnes",
        |      "fields": [
        |        {
        |          "id": "nameOfBusiness",
        |          "label": "Name of business | Enw'r busnes",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "accountingPeriodStartDate",
        |          "label": "Accounting period start date | Dyddiad dechrau'r cyfnod cyfrifeg",
        |          "type": "date",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "accountingPeriodEndDate",
        |          "label": "Accounting period end date | Dyddiad diwedd cyfnod Cyfrifeg",
        |          "type": "date",
        |          "mandatory": "false"
        |        }
        |      ]
        |    },
        |    {
        |      "title": "Rate for the period of the Insurance Premium Tax | Gyfradd ar gyfer y cyfnod y Dreth Premiwm Yswiriant",
        |      "fields": [
        |        {
        |          "id": "standardRateIPTDueForThisPeriod",
        |          "label": "Standard rate IPT due for this period | Cyfradd safonol IPT sy'n ddyledus am y cyfnod hwn",
        |          "helpText": "You should deduct any standard credits which are due to you | Dylech ddidynnu unrhyw gredydau safonol sydd yn ddyledus i chi",
        |          "format": "positiveNumber(8,2)",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "higherRateIPTDueForThisPeriod",
        |          "label": "Higher rate IPT due for this period | Cyfradd uwch IPT sy'n ddyledus am y cyfnod hwn",
        |          "mandatory": "true"
        |        }
        |      ]
        |    }
        |  ]
        |}
        |""".stripMargin

    val formTemplateJsValue = Json.parse(template)
    val formTemplateJsResult = formTemplateJsValue.validate[FormTemplate]

    formTemplateJsResult match {
      case JsSuccess(formTempl, _) =>
        val result = FormTemplateValidator.validateUniqueFields(formTempl.sections).toEither

        result.left.value canEqual s"Some FieldIds are defined more than once: ${List(FormComponentId("isPremisesAddressBusinessAddress2"), FormComponentId("dutyType"))}"

      case JsError(error) => s"Couldn't convert json to FormTemplate, $error"
    }
  }

  val businessDetailsSection = Section(
    "Business details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("nameOfBusiness"),
        Text(AnyText, Constant("")),
        "Name of business",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("businessAddress"),
        Address(international = false),
        "Business address",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None
  )

  val sectionWithDate = Section(
    "Business details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("nameOfBusiness"),
        Text(AnyText, Constant("")),
        "Name of business",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("startDate"),
        Date(AnyDate, Offset(0), None),
        "Start date",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None
  )

  val sectionWithCheckbox = Section(
    "Business details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("nameOfBusiness"),
        Text(AnyText, Constant("")),
        "Name of business",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None
  )

  val sectionWithRadio = Section(
    "Business details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("nameOfBusiness"),
        Text(AnyText, Constant("")),
        "Name of business",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None
  )

  val sectionWithYesNo = Section(
    "Business details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("nameOfBusiness"),
        Text(AnyText, Constant("")),
        "Name of business",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None
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

    val section = Section(
      "Business details",
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      List(
        FormComponent(
          FormComponentId("nameOfBusiness"),
          Text(AnyText, Constant("")),
          "Name of business",
          None,
          None,
          validIf = None,
          mandatory = false,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        )),
      None
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "succeed to find matching section containing only text field which is not mandatory 2" in {

    val section = Section(
      "Business details",
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      List(
        FormComponent(
          FormComponentId("nameOfBusiness"),
          Text(AnyText, Constant("")),
          "Name of business",
          None,
          None,
          validIf = None,
          mandatory = false,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        )),
      None
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = FormTemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing only text field which is mandatory" in {

    val section = Section(
      "Business details",
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      List(
        FormComponent(
          FormComponentId("nameOfBusiness"),
          Text(AnyText, Constant("")),
          "Name of business",
          None,
          None,
          validIf = None,
          mandatory = true,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        )),
      None
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

  private lazy val schema = FormTemplateSchema.schema
  private lazy val jsonSchema = FormTemplateSchema.jsonSchema

}
