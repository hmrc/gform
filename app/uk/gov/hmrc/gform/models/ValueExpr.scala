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

package uk.gov.hmrc.gform.models

import cats.Monoid
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.ValueParser

sealed trait ValueExpr

final case class TextExpression(expr: Expr) extends ValueExpr
final case class DateExpression(dateValue: DateValue) extends ValueExpr
final case class ChoiceExpression(selections: List[Int]) extends ValueExpr

object ValueExpr {
  implicit val format: OFormat[ValueExpr] = {
    val reads: Reads[ValueExpr] = Reads { json =>
      json match {
        case JsString(exprAsStr) =>
          ValueParser.validate(exprAsStr) match {
            case Right(expr) => JsSuccess(expr)
            case Left(error) => JsError(error.toString)
          }
        case otherwise => JsError(s"Invalid expression. Expected String, got $otherwise")
      }
    }

    OFormat[ValueExpr](reads, format)
  }
}
