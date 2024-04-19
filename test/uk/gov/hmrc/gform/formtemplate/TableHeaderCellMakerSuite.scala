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

import munit.FunSuite
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TableHeaderCell

class TableHeaderCellMakerSuite extends FunSuite {

  test(
    "TableHeaderCellMaker should correctly handle non-JSON object input by returning a TableHeaderCell with the provided label"
  ) {
    val inputJson: JsValue = Json.toJson("header")

    val res: Opt[TableHeaderCell] = new TableHeaderCellMaker(inputJson).optTableHeaderCell()
    val expected = TableHeaderCell(label = toSmartString("header"), None)

    assertEquals(res, Right(expected))
  }

  test(
    "TableHeaderCellMaker should correctly handle JSON input with an 'en' attribute by returning a TableHeaderCell with the label from the 'en' attribute"
  ) {
    val inputJson: JsValue = Json.obj("en" -> "en header")

    val res: Opt[TableHeaderCell] = new TableHeaderCellMaker(inputJson).optTableHeaderCell()
    val expected = TableHeaderCell(label = toSmartString("en header"), None)

    assertEquals(res, Right(expected))
  }

  test(
    "TableHeaderCellMaker should correctly handle JSON input with 'en' and 'cy' attributes and a 'classes' attribute by returning a TableHeaderCell with labels from both 'en' and 'cy' attributes, along with the specified classes"
  ) {
    val inputJson: JsValue = Json.obj("en" -> "en header", "cy" -> "cy header", "classes" -> "right")

    val res: Opt[TableHeaderCell] = new TableHeaderCellMaker(inputJson).optTableHeaderCell()
    val expected = TableHeaderCell(label = toSmartString("en header", "cy header"), Some("right"))

    assertEquals(res, Right(expected))
  }

  test(
    "TableHeaderCellMaker should return an error when the input JSON object does not have an 'en' attribute"
  ) {
    val inputJson: JsValue = Json.obj()

    val res: Opt[TableHeaderCell] = new TableHeaderCellMaker(inputJson).optTableHeaderCell()

    assertEquals(res, Left(UnexpectedState("'en' is missing in table header")))
  }
}
