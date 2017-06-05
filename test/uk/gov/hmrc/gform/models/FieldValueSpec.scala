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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import play.api.libs.json.{ Reads, _ }
import uk.gov.hmrc.gform.Spec

class FieldValueSpec extends Spec {

  "FieldValue json object" should "parse as Text if it not include 'type' field" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'text' type without total if no total specified" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'text' type without total if total false specified" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "total": "false"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'text' type with total specified" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "total": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = true), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'text' type including value without total if total false specified" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "value": "Ahah",
         |  "total": "false"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant("Ahah"), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'text' type including value with total specified" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "text",
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "readonly": "true",
         |  "mandatory": "true",
         |  "value": "Ahah",
         |  "total": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant("Ahah"), total = true), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse as Text with 'mandatory' true as mandatory" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "true"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse as Text with 'mandatory' false as not mandatory" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "false"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = false, editable = true, submissible = true))
  }

  it should "parse as Text without 'mandatory' as mandatory" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse as Text without 'submitMode' as editable and submissible" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse as Text with 'submitMode' standard as editable and submissible" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "standard"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse as Text with 'submitMode' readonly as non-editable and submissible" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "readonly"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = false, submissible = true))
  }

  it should "parse as Text with 'submitMode' info as non-editable and non-submissible" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "submitMode": "info"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = true, editable = false, submissible = false))
  }

  it should "parse as Text with 'mandatory' false and 'submitMode' info as non-mandatory, non-editable and non-submissible" in {
    val fieldValue = toFieldValue(
      """|{
         |  "id": "regNum",
         |  "label": "Registration number",
         |  "mandatory": "false",
         |  "submitMode": "info"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("regNum"), Text(Constant(""), total = false), "Registration number", None, mandatory = false, editable = false, submissible = false))
  }

  it should "parse 'choice' type as Radio with Vertical orientation if no multivalue & no format is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no & format=vertical' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"vertical"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Radio with Horizontal orientation if 'multivalue=no & format=horizontal' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"no",
         |  "format":"horizontal"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Radio, NonEmptyList("Natural gas", List("Other gas")), Horizontal, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Vertical orientation if 'multivalue=yes & format=vertical' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"vertical"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Vertical, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as Checkbox with Horizontal orientation if 'multivalue=yes & format=horizontal' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"yes",
         |  "format":"horizontal"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("dutyType"),
        Choice(Checkbox, NonEmptyList("Natural gas", List("Other gas")), Horizontal, List.empty[Int], None),
        "Select the tax type",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as YesNo if 'format=yesno' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as YesNo if 'format=yesno' and 'value=1' are provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "value": "1"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List(1), None),
        "Gas tax type?",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as YesNo if 'format=yesno & multivalue=no' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"no"
         |}"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("taxType"),
        Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None),
        "Gas tax type?",
        None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  it should "parse 'choice' type as YesNo ignoring 'choices' if they are provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ]
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("taxType"), Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None), "Gas tax type?", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'choice' type as YesNo even though 'multivalue=yes' is provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"taxType",
         |  "label":"Gas tax type?",
         |  "format":"yesno",
         |  "multivalue":"yes"
         |}"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("taxType"), Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None), "Gas tax type?", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'choice' type as inline" in {
    val fieldValue = toFieldValue(
      """{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": ["Yes","No"]
         }"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("haveIncludedInvoice"), Choice(Inline, NonEmptyList.of("Yes", "No"), Horizontal, List.empty[Int], None), "Original invoice from the supplier", None, mandatory = true, editable = true, submissible = true))
  }

  it should "parse 'choice' type as inline with value" in {
    val fieldValue = toFieldValue(
      """{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": ["Yes","No", "Not sure"],
           "value": "1"
         }"""
    )

    fieldValue should beJsSuccess(FieldValue(FieldId("haveIncludedInvoice"), Choice(Inline, NonEmptyList.of("Yes", "No", "Not sure"), Horizontal, List(1), None), "Original invoice from the supplier", None, mandatory = true, editable = true, submissible = true))
  }

  it should "faile parse 'choice' type when not enough choices" in {
    val fieldValue = toFieldValue(
      """{
           "type": "choice",
           "id": "haveIncludedInvoice",
           "label": "Original invoice from the supplier",
           "format": "inline",
           "choices": []
         }"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if no 'options' are provided" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'multivalue' is not 'yes' or 'no'" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "multivalue":"wrong-value"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'format' is not 'vertical' or 'horizontal' or 'yesno'" in {
    val fieldValue = toFieldValue(
      """|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "format":"wrong-value"
         |}"""
    )

    fieldValue should be(jsError)
  }

  it should "parse 'file upload' " in {
    val fieldValue = toFieldValue(
      """{
           "type": "fileUpload",
           "id":"attachment1",
           "label": "Attach evidence of your income"
         }"""
    )

    fieldValue should beJsSuccess(
      FieldValue(
        FieldId("attachment1"),
        FileUpload(),
        label = "Attach evidence of your income",
        helpText = None,
        mandatory = true,
        editable = true,
        submissible = true
      )
    )
  }

  private def toFieldValue(template: String): JsResult[FieldValue] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FieldValue]].reads(templateAsJson)
  }
}
