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

import cats.implicits._
import play.api.libs.json.Reads._
import play.api.libs.json._
import scala.util.Try
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormKind, SectionNumber, TaskNumber, TaskSectionNumber }

sealed trait VisitIndex extends Product with Serializable

object VisitIndex {

  final case class Classic(visitsIndex: Set[SectionNumber.Classic]) extends VisitIndex
  final case class TaskList(visitsIndex: Map[Coordinates, Set[SectionNumber.Classic]]) extends VisitIndex

  val key: String = "visitsIndex"

  def empty(formKind: FormKind): VisitIndex =
    formKind.fold[VisitIndex](_ => Classic(Set.empty))(_ => TaskList(Map.empty))

  implicit val format: OFormat[VisitIndex] = OFormat(
    (jsValue: JsValue) =>
      (jsValue \ key).toOption match {
        case None => JsError(s"Missing '$key' field. Failed to decode VisitIndex from: $jsValue")
        case Some(a: JsArray) =>
          a.validate[Set[SectionNumber.Classic]] match {
            case JsSuccess(visits, _) => JsSuccess(Classic(visits))
            case JsError(errors)      => JsSuccess(Classic(Set.empty[SectionNumber.Classic]))
          }
        case Some(o: JsObject) =>
          o.value.toList
            .traverse { case (k, v) =>
              Try(k.split(",").toList).collect { case taskSectionNumber :: taskNumber :: Nil =>
                val key = Coordinates(TaskSectionNumber(taskSectionNumber.toInt), TaskNumber(taskNumber.toInt))
                val visits = v.validate[Set[SectionNumber.Classic]].getOrElse(Set.empty[SectionNumber.Classic])
                key -> visits
              }
            }
          res.fold(
            error => JsSuccess(TaskList(Map.empty)),
            xs => JsSuccess(TaskList(xs.toMap))
          )
        case Some(unexpected) => JsError("Unknown type. Failed to decode VisitIndex from json: " + unexpected)
      },
    (visitIndex: VisitIndex) =>
      visitIndex match {
        case Classic(visitsIndex) => Json.obj(key -> Json.toJson(visitsIndex))
        case TaskList(visitsIndex) =>
          val s: Map[String, JsValue] =
            visitsIndex.toList.map { case (Coordinates(TaskSectionNumber(tsc), TaskNumber(tn)), indexes) =>
              List(tsc, tn).mkString(",") -> {
                Json.toJson(indexes)
              }
            }.toMap
          Json.obj(key -> Json.toJson(s))
      }
  )
}
