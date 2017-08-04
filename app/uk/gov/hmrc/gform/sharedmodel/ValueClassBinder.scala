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

package uk.gov.hmrc.gform.sharedmodel

import cats.implicits._
import play.api.libs.json.{ JsError, JsString, JsSuccess, Reads }
import play.api.mvc.PathBindable
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRawId, SectionNumber }

import scala.util.Try

object ValueClassBinder {

  implicit val formTemplateIdBinder: PathBindable[FormTemplateId] = valueClassBinder(_.value)
  implicit val formTemplateRawIdBinder: PathBindable[FormTemplateRawId] = valueClassBinder(_.value)

  implicit val formIdBinder: PathBindable[FormId] = valueClassBinder(_.value)
  implicit val userIdBinder: PathBindable[UserId] = valueClassBinder(_.value)

  implicit val sectionNumberBinder: PathBindable[SectionNumber] = new PathBindable[SectionNumber] {
    override def bind(key: String, value: String): Either[String, SectionNumber] = Try { SectionNumber(value.toInt) }.map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    override def unbind(key: String, sectionNumber: SectionNumber): String = sectionNumber.value.toString
  }

  def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) = {

    def parseString(str: String) = {
      JsString(str).validate[A] match {
        case JsSuccess(a, _) => Right(a)
        case JsError(error) => Left(s"No valid value in path: $str. Error: $error")
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
