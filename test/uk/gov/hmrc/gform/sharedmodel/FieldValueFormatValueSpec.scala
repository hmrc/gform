/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FieldValueFormatValueSpec extends Spec {

  "FieldValue json object" should "parse a sum expression" in {
    val fieldValue = toFieldValue("""{
           "id": "sum",
           "label": "Label",
           "value": "${amountA + amountB}",
           "format": "shortText"
          }
      """)
    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("sum"),
        Text(ShortText.default, Add(FormCtx(FormComponentId("amountA")), FormCtx(FormComponentId("amountB")))),
        toSmartString("Label"),
        None,
        None,
        None,
        None,
        true,
        true,
        true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))

  }

  // I expect that this character set should be wider, see "Gform Expressions" in Confluence
  it should "parse all these characters into a Constant" in {
    val fieldValue = toFieldValue("""{
           "id": "constant",
           "label": "Label",
           "value": "'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_ ,'",
           "format": "shortText"
          }
      """)
    fieldValue should beJsSuccess(
      FormComponent(
        FormComponentId("constant"),
        Text(ShortText.default, Constant("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_ ,")),
        toSmartString("Label"),
        None,
        None,
        None,
        None,
        true,
        true,
        true,
        derived = false,
        onlyShowOnSummary = false,
        None
      ))

  }

  private def toFieldValue(template: String): JsResult[FormComponent] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FormComponent]].reads(templateAsJson)
  }
}
