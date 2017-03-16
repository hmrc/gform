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
import play.api.libs.functional.syntax._
import uk.gov.hmrc.bforms.models.FormTemplate

sealed trait Expr

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: String) extends Expr
final case class EeittCtx(value: String) extends Expr
final case class Constant(value: String) extends Expr

sealed trait Operation
final case object Addition extends Operation
final case object Multiplication extends Operation

sealed trait Context
final case object FormContext extends Context
final case object AuthContext extends Context
final case object EeittContext extends Context

object Expr {

  implicit val format: OFormat[Expr] = {
    val formatExpr: OFormat[Expr] = derived.oformat

    val reads: Reads[Expr] = (formatExpr: Reads[Expr]) | Reads { json =>
      json match {
        case JsString(exprAsStr) =>
          Parser.validate(exprAsStr) match {
            case Right(expr) => JsSuccess(expr)
            case Left(error) => JsError(error.toString)
          }
        case otherwise => JsError(s"Invalid expression. Expected String, got $otherwise")
      }
    }

    OFormat[Expr](reads, formatExpr)
  }

  def validate(exprs: List[Expr], formTemplate: FormTemplate) = {

    val fieldNamesIds: List[String] = formTemplate.sections.flatMap(_.fields.map(_.id))

    def checkFields(field1: Expr, field2: Expr): ValidationResult = {
      val checkField1 = checkExpression(field1)
      val checkField2 = checkExpression(field2)
      Monoid[ValidationResult].combineAll(List(checkField1, checkField2))
    }

    def checkExpression(expr: Expr): ValidationResult = {
      expr match {
        case Add(field1, field2) => checkFields(field1, field2)
        case Multiply(field1, field2) => checkFields(field1, field2)
        case FormCtx(value) =>
          if (fieldNamesIds.contains(value))
            Valid
          else
            Invalid(s"Form field '$value' is not defined in form template.")
        case AuthCtx(value) => Valid
        case EeittCtx(value) => Valid
        case Constant(_) => Valid
      }
    }

    def checkExpressions(exprs: List[Expr]): ValidationResult = {
      val results = exprs.map(checkExpression)
      Monoid[ValidationResult].combineAll(results)
    }
    checkExpressions(exprs)
  }
}
