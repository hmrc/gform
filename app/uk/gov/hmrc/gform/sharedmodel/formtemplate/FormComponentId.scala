/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.Eq
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

import scala.util.matching.Regex

case class FormComponentId(value: String) extends AnyVal {
  override def toString = value

  def withSuffix(suffix: String): FormComponentId = FormComponentId(value + "-" + suffix)
}

object FormComponentId {

  implicit val catsEq: Eq[FormComponentId] = Eq.fromUniversalEquals

  implicit val vformat: Format[FormComponentId] =
    ValueClassFormat.validatedvformat("id", validate, x => JsString(x.value))

  val oformat: OFormat[FormComponentId] = ValueClassFormat.oformat("id", FormComponentId.apply, _.value)

  val unanchoredIdValidation: Regex = """[_a-zA-Z]\w*""".r
  val anchoredIdValidation: Regex = unanchoredIdValidation.anchored

  private def validate(s: String): JsResult[FormComponentId] =
    if (anchoredIdValidation.findFirstIn(s).isDefined) JsSuccess(FormComponentId(s))
    else
      JsError(
        "Form Component Ids cannot contain any special characters other than an underscore. They also must not start " +
          "with a number.")

}
