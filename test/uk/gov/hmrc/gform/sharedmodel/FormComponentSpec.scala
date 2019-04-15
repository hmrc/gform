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

package uk.gov.hmrc.gform.sharedmodel

import cats.data.NonEmptyList
import play.api.libs.json.{ Reads, _ }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ShortText, _ }

class FormComponentSpec extends Spec {

  "FieldValue json object" should "parse as Text if it not include 'type' field" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'text' type without total if no total specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'text' type without total if total false specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "total": "false"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'text' type with total specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "total": "true"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'text' type including value without total if total false specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "value": "'Ahah'",
         |  "total": "false"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Constant("Ahah")),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'text' type including value with total specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "value": "'Ahah'",
         |  "total": "true"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Constant("Ahah")),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text with 'mandatory' true as mandatory" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "true"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text with 'mandatory' false as not mandatory" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "false"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = false,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text without 'mandatory' as mandatory" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text without 'submitMode' as editable and submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text with 'submitMode' standard as editable and submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "standard"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text with 'submitMode' readonly as non-editable and submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "readonly"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = false,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Text with 'submitMode' info as non-editable and non-submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "info"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = true,
        editable = false,
        submissible = false,
        derived = false,
        errorMessage = None
      ))
  }
  it should "parse as Text with 'submitMode' derived as non-editable and submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "value": "1",
         |  "submitMode": "derived"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List(1), None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = false,
        submissible = true,
        derived = true,
        onlyShowOnSummary = false,
        None
      ))
  }
  it should "throw an error with 'submitMode' derived where there is no Value" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "submitMode": "derived"
         |}""")

    fieldValue shouldBe jsError
  }

  it should "throw an error with 'submitMode' derived where there the Value is not Value" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "submitMode": "derived",
         |  "value": "I am an Invalid value"
         |}""")

    fieldValue shouldBe jsError
  }

  it should "parse as Text with 'mandatory' false and 'submitMode' info as non-mandatory, non-editable and non-submissible" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "false",
         |  "submitMode": "info"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = false,
        editable = false,
        submissible = false,
        derived = false,
        errorMessage = None
      ))
  }
  it should "parse as Text with 'submitMode' notsubmitted  non-submissible and editable" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "notsubmitted"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        None,
        None,
        mandatory = false,
        editable = true,
        submissible = false,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Date with 'summaryinfoonly' info as mandatory, non-editable and submissible" in {
    val fieldValue = toFieldValue("""|{
         | "id": "aprilDate",
         | "type": "date",
         | "label": "Enter a date in April 2017",
         |  "helpText": "For example, 10 4 2017",
         |  "mandatory": "true",
         |  "format": "after 2017-03-31,before 2017-05-01",
         |  "value": "2017-04-10",
         |  "submitMode": "summaryinfoonly"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("aprilDate"),
        Date(
          DateConstraints(List(
            DateConstraint(After, ConcreteDate(ExactYear(2017), ExactMonth(3), ExactDay(31)), OffsetDate(0)),
            DateConstraint(Before, ConcreteDate(ExactYear(2017), ExactMonth(5), ExactDay(1)), OffsetDate(0))
          )),
          Offset(0),
          Some(ExactDateValue(2017, 4, 10))
        ),
        "Enter a date in April 2017",
        Some("For example, 10 4 2017"),
        None,
        None,
        mandatory = true,
        editable = false,
        submissible = false,
        derived = false,
        onlyShowOnSummary = true,
        errorMessage = None
      ))
  }

  it should "throw an error with 'summaryinfoonly' with no value" in {
    val fieldValue = toFieldValue("""|{
         | "id": "aprilDate",
         | "type": "date",
         | "label": "Enter a date in April 2017",
         |  "helpText": "For example, 10 4 2017",
         |  "mandatory": "true",
         |  "format": "after 2017-03-31,before 2017-05-01",
         |  "submitMode": "summaryinfoonly"
         |}""")

    fieldValue shouldBe jsError
  }

  it should "throw an error with 'summaryinfoonly' with an invalid value" in {
    val fieldValue = toFieldValue("""|{
           | "id": "aprilDate",
           | "type": "date",
           | "label": "Enter a date in April 2017",
           |  "helpText": "For example, 10 4 2017",
           |  "mandatory": "true",
           |  "format": "after 2017-03-31,before 2017-05-01",
           |  "value": "I am a invalid value LalaLa",
           |  "submitMode": "summaryinfoonly"
           |}""")

    fieldValue shouldBe jsError
  }

  it should "throw an error with type set to an invalid Value" in {

    assertThrows[Exception] {
      toFieldValue("""|{
           |  "type": "I am a invalid Value",
           |  "id": "homeAddress",
           |  "label": "Home"
           |}""")
    }
  }

  it should "Default to text if no type is set" in {
    val fieldValue = toFieldValue("""|{
         |  "id": "homeAddress",
         |  "label": "Home"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("homeAddress"),
        Text(ShortText, Value),
        "Home",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }
  it should "parse as Address with 'international' false  when not specified" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "address",
         |  "id": "homeAddress",
         |  "label": "Home"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("homeAddress"),
        Address(international = false),
        "Home",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Address with 'international' true when specified yes" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "address",
         |  "id": "homeAddress",
         |  "label": "Home",
         |  "international" : "Yes"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("homeAddress"),
        Address(international = true),
        "Home",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Address with 'international' false when specified no" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "address",
         |  "id": "homeAddress",
         |  "label": "Home",
         |  "international" : "No"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("homeAddress"),
        Address(international = false),
        "Home",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'choice' type as Radio with Vertical orientation if no multivalue & no format is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no & format=vertical' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"vertical"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Radio with Horizontal orientation if 'multivalue=no & format=horizontal' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"horizontal"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Horizontal, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes & format=vertical' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"vertical"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as Checkbox with Horizontal orientation if 'multivalue=yes & format=horizontal' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"horizontal"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Horizontal, List.empty[Int], None),
        "Select the tax type",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as YesNo if 'format=yesno' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as YesNo if 'format=yesno' and 'value=1' are provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "value": "1"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List(1), None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as YesNo if 'format=yesno & multivalue=no' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"no"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse 'choice' type as YesNo ignoring 'choices' if they are provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'choice' type as YesNo even though 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"yes"
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'choice' type as inline" in {
    val fieldValue = toFieldValue("""{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": ["Yes","No"]
         }""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("haveIncludedInvoice"),
        Choice(Inline, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Original invoice from the supplier",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse 'choice' type as inline with value" in {
    val fieldValue = toFieldValue("""{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": ["Yes","No", "Not sure"],
           "value": "1"
         }""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("haveIncludedInvoice"),
        Choice(Inline, NonEmptyList.of("Yes", "No", "Not sure"), Horizontal, List(1), None),
        "Original invoice from the supplier",
        None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "faile parse 'choice' type when not enough choices" in {
    val fieldValue = toFieldValue("""{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": []
         }""")

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if no 'options' are provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type"
         |}""")

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'multivalue' is not 'yes' or 'no'" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"wrong-value"
         |}""")

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'format' is not 'vertical' or 'horizontal' or 'yesno'" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "format":"wrong-value"
         |}""")

    fieldValue should be(jsError)
  }

  it should "parse 'file upload' " in {
    val fieldValue = toFieldValue("""{
           "type": "file",
           "id":"attachment1",
           "label": "Attach evidence of your income"
         }""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("attachment1"),
        FileUpload(),
        label = "Attach evidence of your income",
        helpText = None,
        None,
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse info field and select Standard type if infoType not provided" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "standardInfoFieldID",
                                    |  "label": "Label -- standard info field",
                                    |  "infoText": "This is a sample text for a standard info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("standardInfoFieldID"),
        InformationMessage(StandardInfo, "This is a sample text for a standard info field"),
        "Label -- standard info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse info field and correctly build a standard type info field" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "standardInfoFieldID",
                                    |  "label": "Label -- standard info field",
                                    |  "infoType" : "standard",
                                    |  "infoText": "This is a sample text for a standard info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("standardInfoFieldID"),
        InformationMessage(StandardInfo, "This is a sample text for a standard info field"),
        "Label -- standard info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse the validIf and correctly set it to it's Boolean Expression" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "standardInfoFieldID",
                                    |  "label": "Label -- standard info field",
                                    |  "validIf" : "${info='Hello'}",
                                    |  "infoType" : "standard",
                                    |  "infoText": "This is a sample text for a standard info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("standardInfoFieldID"),
        InformationMessage(StandardInfo, "This is a sample text for a standard info field"),
        "Label -- standard info field",
        None,
        None,
        Some(ValidIf(Equals(FormCtx("info"), Constant("Hello")))),
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse info field and correctly build a long type info field" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "long",
                                    |  "infoText": "This is a sample text for an info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("fieldID"),
        InformationMessage(LongInfo, "This is a sample text for an info field"),
        "Label -- info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse info field and correctly build an important type info field" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "important",
                                    |  "infoText": "This is a sample text for an info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("fieldID"),
        InformationMessage(ImportantInfo, "This is a sample text for an info field"),
        "Label -- info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "parse info field and correctly build a banner type info field" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "banner",
                                    |  "infoText": "This is a sample text for an info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("fieldID"),
        InformationMessage(BannerInfo, "This is a sample text for an info field"),
        "Label -- info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }
  it should "parse info field and correctly build a noformat type info field" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "noformat",
                                    |  "infoText": "This is a sample text for an info field"
                                    |}
      """.stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("fieldID"),
        InformationMessage(NoFormat, "This is a sample text for an info field"),
        "Label -- info field",
        None,
        None,
        None,
        true,
        false,
        false,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))
  }

  it should "fail to parse if info field is not one of a valid types" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "beg_your_pardon?",
                                    |  "infoText": "This is a sample text for an info field"
                                    |}
      """.stripMargin)

    fieldValue should be(jsError)
  }

  it should "fail to parse info field if infoText argument is missing" in {
    val fieldValue = toFieldValue("""
                                    |{
                                    |  "type": "info",
                                    |  "id": "fieldID",
                                    |  "label": "Label -- info field",
                                    |  "infoType" : "beg_your_pardon?"
                                    |}
      """.stripMargin)

    fieldValue should be(jsError)
  }

  it should "parse as Text field with shortName" in {
    val shortName = "This is very very very very very very very shortName"
    val fieldValue = toFieldValue(s"""|{
                                      |  "id": "regNum",
                                      |  "type" : "text",
                                      |  "label": "Registration number",
                                      |  "shortName" : "$shortName",
                                      |  "readonly": "true",
                                      |  "mandatory": "true"
                                      |}""".stripMargin)

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("regNum"),
        Text(ShortText, Value),
        "Registration number",
        None,
        Some(shortName),
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Address with shortName" in {
    val shortName = "This is very very very very very very very shortName"
    val fieldValue = toFieldValue(s"""|{
     |  "type": "address",
     |  "id": "homeAddress",
     |  "label": "Home",
     |  "shortName" : "$shortName"
     |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("homeAddress"),
        Address(international = false),
        "Home",
        None,
        Some(shortName),
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Choice field shortName" in {
    val shortName = "This is very very very very very very very shortName"
    val fieldValue = toFieldValue(s"""{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "shortName" : "$shortName",
           "format": "inline",
           "choices": ["Yes","No", "Not sure"],
           "value": "1"
         }""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("haveIncludedInvoice"),
        Choice(Inline, NonEmptyList.of("Yes", "No", "Not sure"), Horizontal, List(1), None),
        "Original invoice from the supplier",
        None,
        Some(shortName),
        None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ))
  }

  it should "parse as Date constraint to April 17" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "aprilDate",
          "type": "date",
          "label": "Enter a date in April 2017",
          "helpText": "For example, 10 4 2017",
          "mandatory": "true",
          "format": "after 2017-04-02 -2,before 2017-05-02 -1",
          "value": "2017-04-10"
        }
        """)

    fieldValue shouldBe an[JsSuccess[FormComponent]]
  }

  it should "parse as Date constraint to April 17 without specifying offsets" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "aprilDate",
          "type": "date",
          "label": "Enter a date in April 2017",
          "helpText": "For example, 10 4 2017",
          "mandatory": "true",
          "format": "after 2017-03-31,before 2017-05-01",
          "value": "2017-04-10"
        }
        """)

    fieldValue shouldBe an[JsSuccess[FormComponent]]
  }

  it should "parse as Date constrained to next 7 days" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "next7days",
          "type": "date",
          "label": "Enter a date in the next 7 days starting today",
          "helpText": "For example, 10 4 2017",
          "mandatory": "true",
          "format": "after today -1,before today +7"
        }
        """)

    fieldValue shouldBe an[JsSuccess[FormComponent]]
  }

  private def toFieldValue(template: String): JsResult[FormComponent] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FormComponent]].reads(templateAsJson)
  }

}
