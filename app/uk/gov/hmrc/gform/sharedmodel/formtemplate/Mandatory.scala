/*
 * Copyright 2025 HM Revenue & Customs
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

import play.api.libs.json.{ JsError, JsFalse, JsString, JsSuccess, JsTrue, OFormat, Reads }
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser

final case class Mandatory(booleanExpr: BooleanExpr) extends AnyVal

object Mandatory {
  val TRUE: Mandatory = Mandatory(IsTrue)
  val FALSE: Mandatory = Mandatory(IsFalse)

  private val templateReads: Reads[Mandatory] = Reads {
    case JsTrue  => JsSuccess(TRUE)
    case JsFalse => JsSuccess(FALSE)
    case JsString(str) =>
      BooleanExprParser.validate(str) match {
        case Left(unexpectedState) => JsError(unexpectedState.error)
        case Right(be)             => JsSuccess(Mandatory(be))
      }
    case otherwise =>
      JsError(s"Invalid mandatory value. Expected 'true' or 'false', or expression '$${...}', got $otherwise")
  }

  implicit val format: OFormat[Mandatory] = OFormatWithTemplateReadFallback(templateReads)
}
