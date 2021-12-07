/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.parse.Parser
import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess}
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

import scala.util.matching.Regex

case class PageId(id: String) extends AnyVal

object PageId {

  private val idValidation: String = "[_a-zA-Z]\\w*"
  private val anchoredIdValidation: Regex = s"""^$idValidation$$""".r
  val unanchoredIdValidation: Regex = s"""$idValidation""".r
  val unanchoredIdValidationParser: Parser[String] = ???

  implicit val format: Format[PageId] =
    ValueClassFormat.validatedvformat("id", validate, p => JsString(p.id))

  private[formtemplate] def validate(s: String): JsResult[PageId] =
    if (anchoredIdValidation.findFirstIn(s).isDefined) JsSuccess(PageId(s))
    else
      JsError(errorMessage(s))

  private def errorMessage(s: String): String =
    s"Page Ids cannot contain any special characters other than an underscore. They also must not start with a number - '$s'"
}
