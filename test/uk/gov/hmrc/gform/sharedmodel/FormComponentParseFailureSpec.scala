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

import play.api.libs.json.{ Reads, _ }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

class FormComponentParseFailureSpec extends Spec {

  "FieldValue json object" should "not parse as Date if format is wrong" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "startDate",
          "type": "date",
          "label": "Start date",
          "helpText": "For example, 31 3 1980",
          "mandatory": "true",
          "format": "after 2016-09-05 -d",
          "value": "2010-10-10"
        }
        """)

    fieldValue should be(jsError)
  }

  it should "not parse as Date if value is wrong" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "startDate",
          "type": "date",
          "label": "Start date",
          "helpText": "For example, 31 3 1980",
          "mandatory": "true",
          "format": "after 2016-09-05 -2",
          "value": "ldfls-10-10"
        }
        """)

    fieldValue should be(jsError)
  }

  it should "not parse as Date if format and value is wrong" in {
    val fieldValue = toFieldValue(s"""
        {
          "id": "startDate",
          "type": "date",
          "label": "Start date",
          "helpText": "For example, 31 3 1980",
          "mandatory": "true",
          "format": "after 2016-09-05 -d",
          "value": "ldfls-10-10"
        }
        """)

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
         |  "format":"wrong-format"
         |}""")

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'value' is wrong" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "value":"wrong-value"
         |}""")

    fieldValue should be(jsError)
  }

  it should "fail to parse 'choice' type if 'value' and 'format' is wrong" in {
    val fieldValue = toFieldValue("""|{
         |  "type": "choice",
         |  "id":"dutyType",
         |  "label":"Select the tax type",
         |  "choices": [
         |    "Natural gas",
         |    "Other gas"
         |  ],
         |  "value":"wrong-value",
         |  "format":"wrong-format"
         |
         |}""")

    fieldValue should be(jsError)
  }

  it should "fail to parse a text" in {
    val fieldValue = toFieldValue("""{
           "id": "sum",
           "label": "Label",
           "value": "${a - b}"
           ,
           "format": "positiveNumber(3,4,'u'"
          }
      """)

    fieldValue should be(jsError)

  }

  private def toFieldValue(template: String): JsResult[FormComponent] = {

    val templateAsJson = Json.parse(template.stripMargin)

    implicitly[Reads[FormComponent]].reads(templateAsJson)
  }
}
