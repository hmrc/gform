/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormKind, TaskNumber, TaskSectionNumber }

sealed trait VisitIndex extends Product with Serializable

object VisitIndex {

  final case class Classic(visitsIndex: Set[Int]) extends VisitIndex
  final case class TaskList(visitsIndex: Map[(TaskSectionNumber, TaskNumber), Set[Int]]) extends VisitIndex

  val key: String = "visitsIndex"

  def empty(formKind: FormKind): VisitIndex =
    formKind.fold[VisitIndex](_ => Classic(Set.empty))(_ => TaskList(Map.empty))

  implicit val format: OFormat[VisitIndex] = OFormat(
    (jsValue: JsValue) =>
      (jsValue \ key).toOption match {
        case None             => JsError(s"Missing '$key' field. Failed to decode VisitIndex from: $jsValue")
        case Some(a: JsArray) => JsSuccess(Classic(a.value.map(_.as[Int]).toSet))
        case Some(o: JsObject) =>
          val res: Try[List[((TaskSectionNumber, TaskNumber), Set[Int])]] =
            o.value.toList.traverse { case (k, v) =>
              Try(k.split(",").toList.map(_.toInt)).collect { case taskSectionNumber :: taskNumber :: Nil =>
                (TaskSectionNumber(taskSectionNumber) -> TaskNumber(taskNumber)) -> v.as[Set[Int]]
              }
            }
          res.fold(
            error => JsError("Failed to decode VisitIndex for TaskList from json: " + jsValue),
            xs => JsSuccess(TaskList(xs.toMap))
          )
        case Some(unexpected) => JsError("Unknown type. Failed to decode VisitIndex from json: " + unexpected)
      },
    (visitIndex: VisitIndex) =>
      visitIndex match {
        case Classic(visitsIndex) => Json.obj(key -> Json.toJson(visitsIndex))
        case TaskList(visitsIndex) =>
          val s: Map[String, JsValue] = visitsIndex.toList.map {
            case ((TaskSectionNumber(tsc), TaskNumber(tn)), indexes) =>
              List(tsc, tn).mkString(",") -> Json.toJson(indexes)
          }.toMap
          Json.obj(key -> Json.toJson(s))
      }
  )
}
