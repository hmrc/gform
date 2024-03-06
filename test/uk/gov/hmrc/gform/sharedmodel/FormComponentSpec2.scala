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
         |  "multivalue":"no",
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
            .map(l => OptionData.IndexBased(toSmartString(l), None, None, None)),
          Vertical,
          List.empty[Int],
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None
        ),
        toSmartString("Select the tax type"),
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
         |  "multivalue":"no",
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
              .ValueBased(toSmartString("Yes", "Iawn"), None, None, None, OptionDataValue.StringBased("foo")),
            OptionData.ValueBased(toSmartString("No", "Na"), None, None, None, OptionDataValue.StringBased("bar"))
          ),
          Vertical,
          List.empty[Int],
          None,
          None,
          None,
          LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
          None,
          None
        ),
        toSmartString("Select the tax type"),
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

  private def toFieldValue(template: String): JsResult[FormComponent] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FormComponent]].reads(templateAsJson)
  }
}
