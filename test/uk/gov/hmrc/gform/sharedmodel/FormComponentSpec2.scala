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

package uk.gov.hmrc.gform.sharedmodel

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormComponentSpec2 extends Spec {

  "FieldValue json object" should "parse 'choice' type as Radio with Vertical orientation if 'multivalue=no & format=vertical' is provided" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |     {
         |       "label": "Natural gas"
         |     },
         |     {
         |       "label": "Other gas"
         |     }
         |  ],
         |  "multivalue":false,
         |  "format":"vertical",
         |  "presentationHint" : "summariseGroupAsGrid"
         |
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(
          Radio,
          NonEmptyList
            .of("Natural gas", "Other gas")
            .map(l => OptionData.IndexBased(toSmartString(l), None, None, None, None)),
          Vertical,
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
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None,
        Some(List(SummariseGroupAsGrid))
      )
    )
  }

  it should "parse 'choice' with choice as ValueBased" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    {
         |      "value": "foo",
         |      "label": {
         |        "en": "Yes",
         |        "cy": "Iawn"
         |      }
         |    },
         |    {
         |      "value": "bar",
         |      "label": {
         |        "en": "No",
         |        "cy": "Na"
         |      }
         |    }
         |  ],
         |  "multivalue":false,
         |  "format":"vertical",
         |  "presentationHint" : "summariseGroupAsGrid"
         |
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(
          Radio,
          NonEmptyList.of(
            OptionData
              .ValueBased(toSmartString("Yes", "Iawn"), None, None, None, OptionDataValue.StringBased("foo"), None),
            OptionData.ValueBased(toSmartString("No", "Na"), None, None, None, OptionDataValue.StringBased("bar"), None)
          ),
          Vertical,
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
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None,
        Some(List(SummariseGroupAsGrid))
      )
    )
  }

  it should "parse 'choice' with hideChoicesSelected" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id": "dutyType",
         |  "label": "Select the tax type",
         |  "choices": [
         |    {
         |      "value": "foo",
         |      "label": "Yes"
         |    },
         |    {
         |      "value": "bar",
         |      "label": "No"
         |    }
         |  ],
         |  "hideChoicesSelected": true
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(
          Radio,
          NonEmptyList.of(
            OptionData.ValueBased(toSmartString("Yes"), None, None, None, OptionDataValue.StringBased("foo"), None),
            OptionData.ValueBased(toSmartString("No"), None, None, None, OptionDataValue.StringBased("bar"), None)
          ),
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
        toSmartString("Select the tax type"),
        false,
        None,
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None,
        None
      )
    )
  }

  it should "parse 'choice' with option summaryValue" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id": "dutyType",
         |  "label": "Select the tax type",
         |  "choices": [
         |    {
         |      "value": "foo",
         |      "label": "Yes",
         |      "summaryValue": "Yes in summary"
         |    },
         |    {
         |      "value": "bar",
         |      "label": "No"
         |    }
         |  ],
         |  "hideChoicesSelected": true
         |}""")

    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("dutyType"),
        Choice(
          Radio,
          NonEmptyList.of(
            OptionData.ValueBased(
              toSmartString("Yes"),
              None,
              None,
              None,
              OptionDataValue.StringBased("foo"),
              Option(toSmartString("Yes in summary"))
            ),
            OptionData.ValueBased(toSmartString("No"), None, None, None, OptionDataValue.StringBased("bar"), None)
          ),
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
        toSmartString("Select the tax type"),
        false,
        None,
        None,
        None,
        validIf = None,
        mandatory = true,
        editable = true,
        submissible = true,
        derived = false,
        onlyShowOnSummary = false,
        None,
        None
      )
    )
  }

  private def toFieldValue(template: String): JsResult[FormComponent] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FormComponent]].reads(templateAsJson)
  }
}
