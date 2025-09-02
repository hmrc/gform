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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser

final case class DisplayInSummary(booleanExpression: BooleanExpr) extends AnyVal

object DisplayInSummary {

  private val templateReads: Reads[DisplayInSummary] = Reads {
    case JsTrue  => JsSuccess(DisplayInSummary(IsTrue))
    case JsFalse => JsSuccess(DisplayInSummary(IsFalse))
    case JsString(str) =>
      BooleanExprParser.validate(str) match {
        case Left(unexpectedState) => JsError(unexpectedState.error)
        case Right(be)             => JsSuccess(DisplayInSummary(be))
      }
    case otherwise =>
      JsError(s"Invalid displayInSummary value. Expected 'true' or 'false', or expression '$${...}', got $otherwise")
  }

  implicit val format: OFormat[DisplayInSummary] = OFormatWithTemplateReadFallback(templateReads)
}
