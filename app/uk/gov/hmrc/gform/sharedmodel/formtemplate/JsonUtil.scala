/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.reflect.runtime.universe.TypeTag

trait JsonUtils {

  //the NonEmptyList play-json (de)serialisation below is from (except minor change):
  //https://gist.github.com/ferhtaydn/c879e5bc89cc4f61b38802ad6d762222
  def reads[T: Reads: TypeTag]: Reads[NonEmptyList[T]] =
    Reads
      .of[List[T]]
      .collect(JsonValidationError(
        s"expected a NonEmptyList of ${implicitly[TypeTag[T]].tpe} but got an empty list instead")) {
        case head :: tail => NonEmptyList(head, tail)
      }

  def writes[T: Writes]: Writes[NonEmptyList[T]] =
    Writes.of[List[T]].contramap(_.toList)

  implicit def nelFormat[T: Format: TypeTag]: Format[NonEmptyList[T]] =
    Format(reads, writes)

  def valueClassReads[C, V](construct: V => C)(implicit vReads: Reads[V]): Reads[C] = new Reads[C] {
    override def reads(json: JsValue): JsResult[C] = vReads.reads(json).map(construct)
  }

  def valueClassWrites[C, V](extract: C => V)(implicit vWrites: Writes[V]): Writes[C] = new Writes[C] {
    override def writes(o: C): JsValue = vWrites.writes(extract(o))
  }

  def valueClassFormat[C, V: Format](construct: V => C, extract: C => V): Format[C] =
    Format(valueClassReads(construct), valueClassWrites(extract))

  def formatMap[A, B: Format](stringToA: String => A, aToString: A => String): Format[Map[A, B]] =
    implicitly[Format[Map[String, B]]].inmap(_.map {
      case (k, v) => stringToA(k) -> v
    }, _.map {
      case (k, v) => aToString(k) -> v
    })

  def constReads[A](a: A): Reads[A] = new Reads[A] {
    def reads(json: JsValue): JsResult[A] = JsSuccess(a)
  }
}

object JsonUtils extends JsonUtils
