/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import munit.FunSuite
import play.api.libs.json._
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, LayoutDisplayWidth, SummarySection }

class SummarySectionMakerSuite extends FunSuite {
  test(
    "SummarySectionMaker should correctly handle JSON input by returning a SummarySection with excludeFromPdf"
  ) {
    val inputJson: JsValue = Json.parse("""
                                          |{
                                          |  "title": "Check your answers",
                                          |  "header": "Summary header",
                                          |  "footer": "Summary footer",
                                          |  "continueLabel": "Accept and submit",
                                          |  "fields": [
                                          |      {
                                          |        "id": "info1",
                                          |        "type": "info",
                                          |        "label": "label 1",
                                          |        "excludeFromPdf": true,
                                          |        "infoText": "info text 1",
                                          |        "infoType": "noformat"
                                          |      },
                                          |      {
                                          |        "id": "info2",
                                          |        "type": "info",
                                          |        "label": "label 2",
                                          |        "excludeFromPdf": false,
                                          |        "infoText": "info text 2",
                                          |        "infoType": "noformat"
                                          |      },
                                          |      {
                                          |        "id": "info3",
                                          |        "type": "info",
                                          |        "label": "label 3",
                                          |        "excludeFromPdf": true,
                                          |        "infoText": "info text 3",
                                          |        "infoType": "noformat"
                                          |      }
                                          |    ]
                                          |}
                                          |""".stripMargin)

    val res: Opt[SummarySection] = new SummarySectionMaker(inputJson).optSummarySection()
    val expected = SummarySection(
      toSmartString("Check your answers"),
      None,
      toSmartString("Summary header"),
      toSmartString("Summary footer"),
      Some(toSmartString("Accept and submit")),
      (inputJson \ "fields").asOpt[List[FormComponent]].flatMap(NonEmptyList.fromList),
      LayoutDisplayWidth.M,
      None,
      None,
      Some(List(FormComponentId("info1"), FormComponentId("info3")))
    )

    assertEquals(res, Right(expected))
  }

  test(
    "SummarySectionMaker should correctly handle JSON input by returning a SummarySection"
  ) {
    val inputJson: JsValue = Json.parse("""
                                          |{
                                          |  "title": "Check your answers",
                                          |  "header": "Summary header",
                                          |  "footer": "Summary footer"
                                          |}
                                          |""".stripMargin)

    val res: Opt[SummarySection] = new SummarySectionMaker(inputJson).optSummarySection()
    val expected = SummarySection(
      toSmartString("Check your answers"),
      None,
      toSmartString("Summary header"),
      toSmartString("Summary footer"),
      None,
      None,
      LayoutDisplayWidth.M,
      None,
      None,
      None
    )

    assertEquals(res, Right(expected))
  }
}
