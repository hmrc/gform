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

package uk.gov.hmrc.bforms.core

import cats.Monoid
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.bforms.core.parsers.ValueParser
import uk.gov.hmrc.bforms.models.{ FieldId, FormTemplate }

sealed trait ValueExpr

final case class TextExpression(expr: Expr) extends ValueExpr
final case class DateExpression(dateValue: DateValue) extends ValueExpr
final case class ChoiceExpression(selections: List[Int]) extends ValueExpr

sealed trait DateValue

final case object TodayDateValue extends DateValue
final case class ExactDateValue(year: Int, month: Int, day: Int) extends DateValue
final case class NextDateValue(month: Int, day: Int) extends DateValue
final case class PreviousDateValue(month: Int, day: Int) extends DateValue

object DateValue {
  implicit val format: OFormat[DateValue] = derived.oformat
}

sealed trait Expr {
  def validate(formTemplate: FormTemplate): ValidationResult = {
    val fieldNamesIds: List[FieldId] = formTemplate.sections.flatMap(_.fields.map(_.id))

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = field1.validate(formTemplate)
      val checkField2 = field2.validate(formTemplate)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    this match {
      case Add(field1, field2) => checkFields(field1, field2)
      case Multiply(field1, field2) => checkFields(field1, field2)
      case FormCtx(value) =>
        if (fieldNamesIds.map(_.value).contains(value))
          Valid
        else
          Invalid(s"Form field '$value' is not defined in form template.")
      case AuthCtx(value) => Valid
      case EeittCtx(value) => Valid
      case Constant(_) => Valid
    }
  }
}

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: String) extends Expr
final case class EeittCtx(value: String) extends Expr
final case class Constant(value: String) extends Expr

object Expr {
  implicit val format: OFormat[Expr] = derived.oformat
}

sealed trait Operation
final case object Addition extends Operation
final case object Multiplication extends Operation

sealed trait Context
final case object FormContext extends Context
final case object AuthContext extends Context
final case object EeittContext extends Context

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
