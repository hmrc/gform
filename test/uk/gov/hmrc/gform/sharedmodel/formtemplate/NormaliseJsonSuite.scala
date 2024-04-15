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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsResult, JsSuccess, JsValue, Json }
import scala.io.Source
import uk.gov.hmrc.gform.formtemplate.FormTemplatesControllerRequestHandler

class NormaliseJsonSuite extends FunSuite {

  private def readFile(fileName: String): String =
    Source.fromFile(s"test/resources/templates-to-normalise/$fileName.json").mkString

  private def readAsJson(fileName: String): JsValue = {
    val source = readFile(fileName)
    Json.parse(source)
  }
  val table: List[String] = List(
    "revealing-choice-include-revealing-choice",
    "revealing-choice-with-yesno-choice",
    "choice-in-declaration-section",
    "tasklist-choice",
    "file-upload-with-object-store",
    "choice-with-dynamic-options",
    "choices-in-group",
    "table-comp-in-task-declaration-section",
    "table-comp-in-add-to-list"
  )

  table.foreach { case fileName =>
    val input = readAsJson(fileName)
    val expected = readAsJson(fileName + "-expected")

    val result: JsResult[JsValue] = FormTemplatesControllerRequestHandler.normaliseJSON(input)

    test(s"normalise input $fileName.json to $fileName-expected.json") {
      assertEquals(result, JsSuccess(expected))
    }
  }

}
