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

package uk.gov.hmrc.gform.sharedmodel.form

import munit.FunSuite
import play.api.libs.json.{ JsError, JsResult, JsSuccess, Json, JsonValidationError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, SectionNumber, TaskNumber, TaskSectionNumber, TemplateSectionIndex }

class VisitIndexSuite extends FunSuite {

  test("VisitIndex - Classic") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": ["n1", "n2", "n3"]
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsSuccess(
      VisitIndex.Classic(
        Set(
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
        )
      )
    )

    assertEquals(value, expected)
  }

  test("VisitIndex - TaskList 1") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": {
           |    "0,0": []
           |  }
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsSuccess(
      VisitIndex.TaskList(
        Map(
          Coordinates(TaskSectionNumber(0), TaskNumber(0)) -> Set()
        )
      )
    )

    assertEquals(value, expected)
  }

  test("VisitIndex - TaskList 2") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": {
           |    "1,0": ["n1", "n2", "n3"],
           |    "1,1": ["n3", "n4", "n5"]
           |  }
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsSuccess(
      VisitIndex.TaskList(
        Map(
          Coordinates(TaskSectionNumber(1), TaskNumber(0)) -> Set(
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
          ),
          Coordinates(TaskSectionNumber(1), TaskNumber(1)) -> Set(
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(3)),
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(4)),
            SectionNumber.Classic.NormalPage(TemplateSectionIndex(5))
          )
        )
      )
    )

    assertEquals(value, expected)
  }

  test("Malformed VisitIndex type") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": false
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsError(JsonValidationError("Unknown type. Failed to decode VisitIndex from json: false"))

    assertEquals(value, expected)
  }

  test("TaskList VisitIndex - malformed key") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": {
           |    "0": []
           |  }
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsSuccess(VisitIndex.TaskList(Map.empty))

    assertEquals(value, expected)
  }

  test("TaskList VisitIndex - malformed value") {
    val json =
      Json.parse(
        """|{
           |  "visitsIndex": {
           |    "0,0": false
           |  }
           |}""".stripMargin
      )

    val value: JsResult[VisitIndex] = json.validate[VisitIndex]

    val expected = JsSuccess(VisitIndex.TaskList(Map(Coordinates(TaskSectionNumber(0), TaskNumber(0)) -> Set())))

    assertEquals(value, expected)
  }

  test("VisitIndex.Classic to Json") {
    val classic: VisitIndex = VisitIndex.Classic(
      Set(
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
        SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
      )
    )
    val json = Json.toJson(classic)
    val expected = Json.obj(
      "visitsIndex" -> Json.arr("n1", "n2", "n3")
    )

    assertEquals(json, expected)
  }

  test("VisitIndex.TaskList to Json") {
    val taskList: VisitIndex = VisitIndex.TaskList(
      Map(
        Coordinates(TaskSectionNumber(1), TaskNumber(0)) -> Set(
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(1)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(2)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(3))
        ),
        Coordinates(TaskSectionNumber(1), TaskNumber(1)) -> Set(
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(3)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(4)),
          SectionNumber.Classic.NormalPage(TemplateSectionIndex(5))
        )
      )
    )
    val json = Json.toJson(taskList)
    val expected = Json.obj(
      "visitsIndex" -> Json.obj(
        "1,0" -> Json.arr("n1", "n2", "n3"),
        "1,1" -> Json.arr("n3", "n4", "n5")
      )
    )

    assertEquals(json, expected)
  }

}
