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

package uk.gov.hmrc.gform.builder

import io.circe.CursorOp
import io.circe.CursorOp._
import scala.util.matching.Regex

object SectionPath {
  def apply(value: String) = new SectionPath(value)

  def nonRepeatedHistory(sectionIndex: Int) =
    List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  def nonRepeatedATLHistory(pageIndex: Int, sectionIndex: Int) =
    List.fill(pageIndex)(MoveRight) ::: List(DownArray, DownField("pages")) :::
      List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  def taskListHistory(sectionIndex: Int, taskIndex: Int, taskSectionIndex: Int) =
    List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
      List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
      List.fill(taskSectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  def taskHistory(sectionIndex: Int, taskIndex: Int) =
    List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
      List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))

  def taskListATLHistory(pageIndex: Int, sectionIndex: Int, taskIndex: Int, taskSectionIndex: Int) =
    List.fill(pageIndex)(MoveRight) ::: List(DownArray, DownField("pages")) :::
      List.fill(sectionIndex)(MoveRight) ::: List(DownArray, DownField("sections")) :::
      List.fill(taskIndex)(MoveRight) ::: List(DownArray, DownField("tasks")) :::
      List.fill(taskSectionIndex)(MoveRight) ::: List(DownArray, DownField("sections"))
}

class SectionPath(val value: String) {
  def toHistory(): List[CursorOp] = value match {
    case sectionPattern(sectionIndex) =>
      SectionPath.nonRepeatedHistory(sectionIndex.toInt)
    case atlPagePattern(sectionIndex, pageIndex) =>
      SectionPath.nonRepeatedATLHistory(pageIndex.toInt, sectionIndex.toInt)
    case taskPattern(taskSectionIndex, taskIndex) =>
      SectionPath.taskHistory(taskSectionIndex.toInt, taskIndex.toInt)
    case taskListSectionPattern(taskSectionIndex, taskIndex, sectionIndex) =>
      SectionPath.taskListHistory(sectionIndex.toInt, taskIndex.toInt, taskSectionIndex.toInt)
    case taskListAtlPagePattern(taskSectionIndex, taskIndex, sectionIndex, pageIndex) =>
      SectionPath.taskListATLHistory(pageIndex.toInt, sectionIndex.toInt, taskIndex.toInt, taskSectionIndex.toInt)
    case _ =>
      Nil
  }

  private val sectionPattern: Regex = """^\.sections\[(\d+)\]""".r
  private val atlPagePattern: Regex = """^\.sections\[(\d+)\]\.pages\[(\d+)\]$""".r
  private val taskListSectionPattern: Regex = """^\.sections\[(\d+)\]\.tasks\[(\d+)\]\.sections\[(\d+)\]$""".r
  private val taskListAtlPagePattern: Regex =
    """^\.sections\[(\d+)\]\.tasks\[(\d+)\]\.sections\[(\d+)\]\.pages\[(\d+)\]$""".r
  private val taskPattern: Regex = """^\.sections\[(\d+)\]\.tasks\[(\d+)\]$""".r

}
