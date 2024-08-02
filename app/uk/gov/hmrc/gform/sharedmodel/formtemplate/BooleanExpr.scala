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

import scalax.collection.GraphPredef._
import uk.gov.hmrc.gform.formtemplate.{ BooleanExprId, BooleanExprSubstitutions }
import julienrf.json.derived
import play.api.libs.json._
import scalax.collection.{ Graph, GraphEdge }
import uk.gov.hmrc.gform.exceptions.UnexpectedState

import scala.annotation.tailrec
import scala.util.matching.Regex

sealed trait BooleanExpr
final case class Equals(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThan(left: Expr, right: Expr) extends BooleanExpr
final case class GreaterThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class LessThan(left: Expr, right: Expr) extends BooleanExpr
final case class LessThanOrEquals(left: Expr, right: Expr) extends BooleanExpr
final case class Not(e: BooleanExpr) extends BooleanExpr
final case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
final case object IsTrue extends BooleanExpr
final case object IsFalse extends BooleanExpr
final case class Contains(multiValueField: FormCtx, value: Expr) extends BooleanExpr
final case class DuplicateExists(fieldList: List[FormCtx]) extends BooleanExpr
final case class In(value: Expr, dataSource: DataSource) extends BooleanExpr
final case class MatchRegex(expr: Expr, regex: Regex) extends BooleanExpr

final case class DateBefore(left: DateExpr, right: DateExpr) extends BooleanExpr
final case class DateAfter(left: DateExpr, right: DateExpr) extends BooleanExpr
final case class First(formCtx: FormCtx) extends BooleanExpr
final case class IsLogin(value: LoginInfo) extends BooleanExpr

// Instances of this class are eliminated in substitution process
// So they never appear in MongoDB and thus don't need to be present on gform-frontend
final case class TopLevelRef(booleanExprId: BooleanExprId) extends BooleanExpr

final case class FormPhase(value: FormPhaseValue) extends BooleanExpr
sealed trait FormPhaseValue
case object InstructionPDF extends FormPhaseValue

object FormPhaseValue {
  implicit val format: OFormat[FormPhaseValue] = derived.oformat()
}
object BooleanExpr {
  implicit val format: OFormat[BooleanExpr] = derived.oformat()

  implicit val regexFormat: Format[Regex] = {
    val reads: Reads[Regex] = Reads.of[String].map(_.r)
    val writes: Writes[Regex] = Writes.of[String].contramap(_.toString)
    Format(reads, writes)
  }

  implicit val leafExprs: LeafExpr[BooleanExpr] = (path: TemplatePath, be: BooleanExpr) => {

    def withPath(xs: Expr*): List[ExprWithPath] = xs.toList.map(x => ExprWithPath(path, x))

    def loop(t: BooleanExpr): List[ExprWithPath] = t match {
      case Equals(left: Expr, right: Expr)                 => withPath(left, right)
      case GreaterThan(left: Expr, right: Expr)            => withPath(left, right)
      case GreaterThanOrEquals(left: Expr, right: Expr)    => withPath(left, right)
      case LessThan(left: Expr, right: Expr)               => withPath(left, right)
      case LessThanOrEquals(left: Expr, right: Expr)       => withPath(left, right)
      case Not(e: BooleanExpr)                             => loop(e)
      case Or(left: BooleanExpr, right: BooleanExpr)       => loop(left) ++ loop(right)
      case And(left: BooleanExpr, right: BooleanExpr)      => loop(left) ++ loop(right)
      case IsTrue                                          => Nil
      case IsFalse                                         => Nil
      case Contains(multiValueField: FormCtx, value: Expr) => withPath(value, multiValueField)
      case In(value: Expr, dataSource: DataSource)         => withPath(value)
      case MatchRegex(expr: Expr, regex: Regex)            => withPath(expr)
      case DateBefore(left: DateExpr, right: DateExpr)     => withPath(left.leafExprs ++ right.leafExprs: _*)
      case DateAfter(left: DateExpr, right: DateExpr)      => withPath(left.leafExprs ++ right.leafExprs: _*)
      case FormPhase(value: FormPhaseValue)                => Nil
      case TopLevelRef(_)                                  => Nil
      case First(formCtx)                                  => withPath(formCtx)
      case IsLogin(_)                                      => Nil
      case DuplicateExists(fieldList)                      => withPath(fieldList: _*)
    }
    loop(be)

  }

  private def toGraph(booleanExprSubstitutions: BooleanExprSubstitutions): Graph[BooleanExprId, GraphEdge.DiEdge] = {
    val layers: Iterable[GraphEdge.DiEdge[BooleanExprId]] =
      booleanExprSubstitutions.expressions.flatMap { case (expressionId, expr) =>
        resolveReferences(expr).map(referenceId => expressionId ~> referenceId)
      }
    layers.foldLeft(Graph.empty[BooleanExprId, GraphEdge.DiEdge])(_ union Set(_))
  }

  def resolveReferences(
    booleanExprSubstitutions: BooleanExprSubstitutions
  ): Either[UnexpectedState, BooleanExprSubstitutions] = {
    val graph: Graph[BooleanExprId, GraphEdge.DiEdge] = toGraph(booleanExprSubstitutions)
    val selfReferencingExpressions =
      graph.edges.filter(edge => edge.source == edge.target).flatMap(_.value.map(_.value))
    if (selfReferencingExpressions.nonEmpty)
      Left(UnexpectedState(s"The booleanExpression ${selfReferencingExpressions.head.id} cannot reference itself"))
    else
      Right(booleanExprSubstitutions)
  }

  private def toBooleanExprIds(xs: BooleanExpr*): List[BooleanExprId] = xs.toList.flatMap(x => resolveReferences(x))

  @tailrec
  private def resolveReferences(t: BooleanExpr): List[BooleanExprId] = t match {
    case Not(e: BooleanExpr)                        => resolveReferences(e)
    case Or(left: BooleanExpr, right: BooleanExpr)  => toBooleanExprIds(left, right)
    case And(left: BooleanExpr, right: BooleanExpr) => toBooleanExprIds(left, right)
    case TopLevelRef(booleanExprId)                 => List(booleanExprId)
    case Equals(_, _)                               => Nil
    case GreaterThan(_, _)                          => Nil
    case GreaterThanOrEquals(_, _)                  => Nil
    case LessThan(_, _)                             => Nil
    case LessThanOrEquals(_, _)                     => Nil
    case IsTrue                                     => Nil
    case IsFalse                                    => Nil
    case FormPhase(_)                               => Nil
    case IsLogin(_)                                 => Nil
    case Contains(_, _)                             => Nil
    case In(_, _)                                   => Nil
    case MatchRegex(_, _)                           => Nil
    case DateBefore(_, _)                           => Nil
    case DateAfter(_, _)                            => Nil
    case First(_)                                   => Nil
    case DuplicateExists(_)                         => Nil
  }
}

object EqualsWithConstant {
  def unapply(be: BooleanExpr): Option[(FormCtx, Constant, Boolean)] =
    be match {
      case Equals(f @ FormCtx(_), c @ Constant(_)) => Some((f, c, false))
      case Equals(c @ Constant(_), f @ FormCtx(_)) => Some((f, c, true))
      case _                                       => None
    }
}
