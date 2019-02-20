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

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.auth.core.{ AffinityGroup => CoreAffinityGroup }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._

import scala.util.{ Failure, Success, Try }

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class NotEquals(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThan(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class LessThan(left: Expr, right: Expr) extends BooleanExpr
final case class LessThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class Not(e: BooleanExpr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr

case class BooleanExprResultWithDependents(
  beResult: Boolean,
  dependingOn: List[FormComponentId]
)

object BooleanExprResultWithDependents {
  def justResult(beResult: Boolean) = BooleanExprResultWithDependents(beResult, List.empty)
}

object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat

  def isTrue(
    expr: BooleanExpr,
    data: Map[FormComponentId, Seq[String]],
    affinityGroup: Option[CoreAffinityGroup]): BooleanExprResultWithDependents = {

    def isTrue0(expr: BooleanExpr): BooleanExprResultWithDependents = isTrue(expr, data, affinityGroup)

    def decimalValue(field: Expr): BigDecimal = {
      val str = stringValue(field)._1.replace(",", "")
      Try(BigDecimal(str)).getOrElse(0)
    }

    def operate(field1: Expr, operator: (BigDecimal, BigDecimal) => BigDecimal, field2: Expr): String =
      operator(decimalValue(field1), decimalValue(field2)).toString()

    def stringValue(expr: Expr): (String, List[FormComponentId]) =
      expr match {
        case Constant(value)             => (value, List.empty)
        case UserCtx(_)                  => (affinityGroupNameO(affinityGroup), List.empty)
        case Add(field1, field2)         => (operate(field1, _ + _, field2), List.empty)
        case Subtraction(field1, field2) => (operate(field1, _ - _, field2), List.empty)
        case Multiply(field1, field2)    => (operate(field1, _ * _, field2), List.empty)
        case id: FormCtx =>
          (data.get(id.toFieldId).flatMap(_.headOption).getOrElse(""), List(id.toFieldId))
        case _ => ("", List.empty)
      }

    def toMaybeBigDecimal(str: String): Option[BigDecimal] =
      Try(BigDecimal(str.replace(",", ""))).toOption

    def compare(
      leftField: Expr,
      bigDecimalRelation: (BigDecimal, BigDecimal) => Boolean,
      stringRelation: (String, String) => Boolean,
      rightField: Expr): BooleanExprResultWithDependents = {
      val (left, xs1) = stringValue(leftField)
      val (right, xs2) = stringValue(rightField)
      (toMaybeBigDecimal(left), toMaybeBigDecimal(right)) match {
        case (Some(l), Some(r)) => BooleanExprResultWithDependents(bigDecimalRelation(l, r), xs1 ++ xs2)
        case _                  => BooleanExprResultWithDependents(stringRelation(left, right), xs1 ++ xs2)
      }
    }

    expr match {
      case Equals(field1, field2)              => compare(field1, _ == _, _ == _, field2)
      case NotEquals(field1, field2)           => compare(field1, _ != _, _ != _, field2)
      case GreaterThan(field1, field2)         => compare(field1, _ > _, _ > _, field2)
      case GreaterThanOrEquals(field1, field2) => compare(field1, _ >= _, _ >= _, field2)
      case LessThan(field1, field2)            => compare(field1, _ < _, _ < _, field2)
      case LessThanOrEquals(field1, field2)    => compare(field1, _ <= _, _ <= _, field2)
      case Not(expr) =>
        val result = isTrue0(expr)
        BooleanExprResultWithDependents(!result.beResult, result.dependingOn)
      case Or(expr1, expr2) =>
        val BooleanExprResultWithDependents(b1, xs1) = isTrue0(expr1)
        if (b1) {
          BooleanExprResultWithDependents(b1, xs1)
        } else isTrue0(expr2)

      case And(expr1, expr2) =>
        val BooleanExprResultWithDependents(b1, xs1) = isTrue0(expr1)
        val BooleanExprResultWithDependents(b2, xs2) = isTrue0(expr2)
        BooleanExprResultWithDependents(b1 & b2, xs1 ++ xs2)
      case IsTrue => BooleanExprResultWithDependents.justResult(true)
      case _      => BooleanExprResultWithDependents.justResult(false)
    }
  }

}

sealed trait Comparison
final case object Equality extends Comparison

sealed trait BooleanOperation
final case object OrOperation extends BooleanOperation
final case object AndOperation extends BooleanOperation
