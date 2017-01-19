/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.binders

import play.api.libs.json.{ JsError, JsString, JsSuccess, Reads }
import play.api.mvc.PathBindable
import uk.gov.hmrc.bforms.model.FormTypeId

object ValueClassBinder {

  implicit val formTypeIdBinder: PathBindable[FormTypeId] = valueClassBinder(_.value)

  def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) = {

    def parseString(str: String) = {
      JsString(str).validate[A] match {
        case JsSuccess(a, _) => Right(a)
        case JsError(_) => Left("No valid value in path: " + str)
      }
    }

    new PathBindable[A] {
      override def bind(key: String, value: String): Either[String, A] =
        stringBinder.bind(key, value).right.flatMap(parseString)

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }
  }
}
