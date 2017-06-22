/*
 * Copyright 2017 HM Revenue & Customs
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
import cats.syntax.either._
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models._

class TemplateValidatorSpec extends Spec {
  "Section.validate" should "validate unique FieldIds" in {
    val template =
      """{
        |  "formTypeId": "IPT100",
        |  "formName": "Insurance Premium Tax Return | Yswiriant Ffurflen Dreth Premiwm",
        |  "version": "0.2.5",
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
        |          "format": "date",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "accountingPeriodEndDate",
        |          "label": "Accounting period end date | Dyddiad diwedd cyfnod Cyfrifeg",
        |          "format": "date",
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
        |          "format": "sterling",
        |          "mandatory": "true"
        |        },
        |        {
        |          "id": "higherRateIPTDueForThisPeriod",
        |          "label": "Higher rate IPT due for this period | Cyfradd uwch IPT sy'n ddyledus am y cyfnod hwn",
        |          "format": "sterling",
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
        val result = Section.validateUniqueFields(formTempl.sections).toEither

        result.left.value canEqual s"Some FieldIds are defined more than once: ${
          List(
            FieldId("isPremisesAddressBusinessAddress2"),
            FieldId("dutyType")
          )
        }"

      case JsError(error) => s"Couldn't convert json to FormTemplate, $error"
    }
  }

  "TemplateValidator.conform" should "validate template against template schema" in {

    val schema =
      """|{
         | "type": "object",
         | "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    },
         |    "sections": {
         |      "type": "array",
         |      "items": {
         |        "type": "object",
         |        "properties": {
         |          "title": {
         |            "type": "string"
         |          }
         |        },
         |        "required": [
         |          "title"
         |        ]
         |      }
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val template =
      """|{
         |  "formTypeId": "IPT100",
         |  "sections": [
         |    {
         |      "title": "Your details | eich manylion"
         |    }
         |  ]
         |}""".stripMargin

    val res =
      for {
        schemaRes <- SchemaValidator.conform(Json.parse(schema).as[Schema])
        tr <- schemaRes.conform(Json.parse(template)).toEither
      } yield tr

    res.right.value should be(())
  }

  it should "validate template with nested arrays against template schema" in {

    val schema =
      """|{
         | "type": "object",
         | "properties": {
         |    "formTypeId": {
         |      "type": "string"
         |    },
         |    "sections": {
         |      "type": "array",
         |      "items": {
         |        "type": "object",
         |        "properties": {
         |          "title": {
         |            "type": "string"
         |          },
         |          "fields": {
         |            "type": "array",
         |            "items": {
         |              "type": "object",
         |              "properties": {
         |                "id": {
         |                  "type": "string"
         |                },
         |                "label": {
         |                  "type": "string"
         |                },
         |                "mandatory": {
         |                  "type": "string"
         |                },
         |                "submitMode": {
         |                  "type": "string"
         |                }
         |              },
         |              "required": [
         |                "id",
         |                "label"
         |              ]
         |            }
         |          }
         |        },
         |        "required": [
         |          "title"
         |        ]
         |      }
         |    }
         |  },
         |  "required": [
         |    "formTypeId"
         |  ]
         |}""".stripMargin

    val template =
      """|{
         |  "formTypeId": "IPT100",
         |  "sections": [
         |    {
         |      "title": "Your details | eich manylion",
         |      "fields": [
         |        {
         |          "id": "iptRegNum",
         |          "label": "Insurance Premium Tax (IPT) registration number | Treth Premiwm Yswiriant (IPT) rhif cofrestru",
         |          "submitMode": "standard",
         |          "mandatory": "true"
         |        }
         |      ]
         |    }
         |  ]
         |}""".stripMargin

    val res =
      for {
        schemaRes <- SchemaValidator.conform(Json.parse(schema).as[Schema])
        tr <- schemaRes.conform(Json.parse(template)).toEither
      } yield tr

    res.right.value should be(())
  }

  val businessDetailsSection = Section(
    "Business details",
    None, None,
    List(
      FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true),
      FieldValue(FieldId("businessAddress"), Address(international = false), "Business address", None, mandatory = true, editable = true, submissible = true)
    )
  )

  val sectionWithDate = Section(
    "Business details",
    None, None,
    List(
      FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true),
      FieldValue(FieldId("startDate"), Date(AnyDate, Offset(0), None), "Start date", None, mandatory = true, editable = true, submissible = true)
    )
  )

  val sectionWithCheckbox = Section(
    "Business details",
    None, None,
    List(
      FieldValue(
        FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true
      ),
      FieldValue(
        FieldId("dutyType"), Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None), "Select the tax type", None, mandatory = true, editable = true, submissible = true
      )
    )
  )

  val sectionWithRadio = Section(
    "Business details",
    None, None,
    List(
      FieldValue(
        FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true
      ),
      FieldValue(
        FieldId("dutyType"), Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None), "Select the tax type", None, mandatory = true, editable = true, submissible = true
      )
    )
  )

  val sectionWithYesNo = Section(
    "Business details",
    None, None,
    List(
      FieldValue(
        FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true
      ),
      FieldValue(
        FieldId("taxType"), Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None), "Gas tax type?", None, mandatory = true, editable = true, submissible = true
      )
    )
  )

  "TemplateValidator.getMatchingSection" should "find matching section containing address component" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("businessAddress-street1"), "street1"),
      FormField(FieldId("businessAddress-street2"), "street2"),
      FormField(FieldId("businessAddress-street3"), "street3"),
      FormField(FieldId("businessAddress-street4"), "street4"),
      FormField(FieldId("businessAddress-postcode"), "postcode"),
      FormField(FieldId("businessAddress-country"), "country")
    )
    val sections = List(businessDetailsSection)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "succeed to find matching section containing address component when optional fields are not present" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("businessAddress-street1"), "street1"),
      FormField(FieldId("businessAddress-postcode"), "postcode")
    )
    val sections = List(businessDetailsSection)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing address component when mandatory fields are not present" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("businessAddress.town"), "town"),
      FormField(FieldId("businessAddress.county"), "county"),
      FormField(FieldId("businessAddress.postcode"), "postcode")
    )
    val sections = List(businessDetailsSection)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "fail to find matching section containing address component when field not in form template is present" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("businessAddress.street1"), "street1"),
      FormField(FieldId("businessAddress.town"), "town"),
      FormField(FieldId("businessAddress.county"), "county"),
      FormField(FieldId("businessAddress.postcode"), "postcode"),
      FormField(FieldId("attacker.injected.field"), "); drop all tables;")
    )
    val sections = List(businessDetailsSection)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "find matching section containing date component" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("startDate.day"), "1"),
      FormField(FieldId("startDate.month"), "12"),
      FormField(FieldId("startDate.year"), "2000")
    )
    val sections = List(sectionWithDate)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing date component when mandatory fields are not present" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("startDate.month"), "12"),
      FormField(FieldId("startDate.year"), "2000")
    )
    val sections = List(sectionWithDate)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "fail to find matching section containing date component when field not in form template is present" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("startDate.day"), "1"),
      FormField(FieldId("startDate.month"), "12"),
      FormField(FieldId("startDate.year"), "2000"),
      FormField(FieldId("attacker.injected.field"), "); drop all tables;")
    )
    val sections = List(sectionWithDate)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "succeed to find matching section containing only text field which is not mandatory" in {

    val section = Section(
      "Business details",
      None, None,
      List(
        FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = false, editable = true, submissible = true)
      )
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "succeed to find matching section containing only text field which is not mandatory 2" in {

    val section = Section(
      "Business details",
      None, None,
      List(
        FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = false, editable = true, submissible = true)
      )
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "fail to find matching section containing only text field which is mandatory" in {

    val section = Section(
      "Business details",
      None, None,
      List(
        FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, mandatory = true, editable = true, submissible = true)
      )
    )

    val formFields = List() // Nothing submitted

    val sections = List(section)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('left)
  }

  it should "find matching section containing Checkbox component" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("dutyType"), "0,1")
    )
    val sections = List(sectionWithCheckbox)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "find matching section containing Radio component" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("dutyType"), "0")
    )
    val sections = List(sectionWithRadio)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }

  it should "find matching section containing YesNo component" in {

    val formFields = List(
      FormField(FieldId("nameOfBusiness"), "Apple inc."),
      FormField(FieldId("taxType"), "0")
    )
    val sections = List(sectionWithYesNo)
    val res = TemplateValidator.getMatchingSection(formFields, sections)

    res should be('right)
  }
}
