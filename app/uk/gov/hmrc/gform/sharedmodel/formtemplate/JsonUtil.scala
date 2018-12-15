/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.{ Format, Reads, Writes }
import scala.reflect.runtime.universe.TypeTag

trait JsonUtils {

  //the NonEmptyList play-json (de)serialisation below is from (except minor change):
  //https://gist.github.com/ferhtaydn/c879e5bc89cc4f61b38802ad6d762222
  def reads[T: Reads: TypeTag]: Reads[NonEmptyList[T]] =
    Reads
      .of[List[T]]
      .collect(
        ValidationError(s"expected a NonEmptyList of ${implicitly[TypeTag[T]].tpe} but got an empty list instead")) {
        case head :: tail => NonEmptyList(head, tail)
      }

  def writes[T: Writes]: Writes[NonEmptyList[T]] =
    Writes.of[List[T]].contramap(_.toList)

  implicit def nelFormat[T: Format: TypeTag]: Format[NonEmptyList[T]] =
    Format(reads, writes)

}

object JsonUtils extends JsonUtils
