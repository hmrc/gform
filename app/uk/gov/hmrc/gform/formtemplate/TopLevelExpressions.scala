/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.syntax.all._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.models.constraints.ReferenceInfo
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object TopLevelExpressions {

  val emptyGraph: Graph[ExpressionId, GraphEdge.DiEdge] = Graph.empty

  def toGraph(
    exprSubstitutions: ExprSubstitutions
  ): Graph[ExpressionId, GraphEdge.DiEdge] = {
    val expressionIdLookup: Set[ExpressionId] = exprSubstitutions.expressions.keys.toSet

    val layers: Iterable[GraphEdge.DiEdge[ExpressionId]] = exprSubstitutions.expressions.flatMap {
      case (expressionId, expr) =>
        val exprsWithPath: List[ExprWithPath] = LeafExpr(TemplatePath.leaf, expr)
        val referenceInfos: List[ReferenceInfo] = exprsWithPath.flatMap(_.referenceInfos)

        referenceInfos.flatMap {
          case ReferenceInfo.FormCtxExpr(_, FormCtx(FormComponentId(value))) =>
            val exId = ExpressionId(value)
            if (expressionIdLookup(exId)) {
              List(expressionId ~> exId)
            } else {
              Nil
            }
          case _ => Nil
        }
    }

    layers.foldLeft(emptyGraph)(_ + _)
  }

  def toTopologicalSort(
    graph: Graph[ExpressionId, GraphEdge.DiEdge]
  ): Either[graph.NodeT, Iterable[(Int, List[uk.gov.hmrc.gform.formtemplate.ExpressionId])]] = {

    def sortedOuterNodes(items: Iterable[graph.NodeT]) =
      items.toList
        .map(_.toOuter)

    graph.topologicalSort.map(_.toLayered.map { case (index, items) =>
      (index, sortedOuterNodes(items))
    })
  }

  def resolveReferences(exprSubstitutions: ExprSubstitutions): Either[UnexpectedState, ExprSubstitutions] = {

    val graph: Graph[ExpressionId, GraphEdge.DiEdge] = toGraph(exprSubstitutions)

    val sort = toTopologicalSort(graph)

    sort.bimap(
      nodeT =>
        UnexpectedState(
          s"Cycle detected in top level expressions. Violating node is '${nodeT.toOuter.id}'. Graph contains cycle: ${graph.findCycle}"
        ),
      iterable =>
        ExprSubstitutions(
          iterable.toList.reverse.foldLeft(exprSubstitutions.expressions) { case (acc0, (index, layerNodes)) =>
            layerNodes.foldLeft(acc0) { case (acc, expr) =>
              resolveExpr(acc, expr)
            }
          }
        )
    )
  }

  def resolveExpr(expressions: Map[ExpressionId, Expr], expressionId: ExpressionId): Map[ExpressionId, Expr] = {

    def loopBooleanExpr(t: BooleanExpr): BooleanExpr = t match {
      case Equals(l, r)                     => Equals(loop(l), loop(r))
      case GreaterThan(l, r)                => GreaterThan(loop(l), loop(r))
      case GreaterThanOrEquals(l, r)        => GreaterThanOrEquals(loop(l), loop(r))
      case LessThan(l, r)                   => LessThan(loop(l), loop(r))
      case LessThanOrEquals(l, r)           => LessThanOrEquals(loop(l), loop(r))
      case Not(e)                           => Not(loopBooleanExpr(e))
      case Or(left, right)                  => Or(loopBooleanExpr(left), loopBooleanExpr(right))
      case And(left, right)                 => And(loopBooleanExpr(left), loopBooleanExpr(right))
      case IsTrue                           => IsTrue
      case IsFalse                          => IsFalse
      case Contains(multiValueField, value) => Contains(multiValueField, loop(value))
      case In(value, dataSource)            => In(loop(value), dataSource)
      case m @ MatchRegex(formCtx, regex)   => m
      case d @ DateBefore(l, r)             => d
      case d @ DateAfter(l, r)              => d
      case f @ FormPhase(value)             => f
      case tl @ TopLevelRef(id)             => tl
    }

    def loop(e: Expr): Expr =
      e match {
        case Else(l, r)                   => Else(loop(l), loop(r))
        case Add(l, r)                    => Add(loop(l), loop(r))
        case Multiply(l, r)               => Multiply(loop(l), loop(r))
        case Subtraction(l, r)            => Subtraction(loop(l), loop(r))
        case Period(l, r)                 => Period(loop(l), loop(r))
        case Sum(l)                       => Sum(loop(l))
        case PeriodExt(p, pe)             => PeriodExt(loop(p), pe)
        case d @ DateCtx(dateExpr)        => d
        case i @ IfElse(cond, l, r)       => IfElse(loopBooleanExpr(cond), loop(l), loop(r))
        case f @ FormCtx(formComponentId) => expressions.getOrElse(ExpressionId(formComponentId.value), e)
        case AddressLens(_, _)            => e
        case AuthCtx(_)                   => e
        case Constant(_)                  => e
        case Count(_)                     => e
        case FormTemplateCtx(_)           => e
        case HmrcRosmRegistrationCheck(_) => e
        case LangCtx                      => e
        case LinkCtx(_)                   => e
        case ParamCtx(_)                  => e
        case PeriodValue(_)               => e
        case UserCtx(_)                   => e
        case Value                        => e
        case DataRetrieveCtx(_, _)        => e
        case Size(_, _)                   => e
      }
    expressions.get(expressionId).fold(expressions) { expr =>
      expressions + (expressionId -> loop(expr))
    }
  }
}