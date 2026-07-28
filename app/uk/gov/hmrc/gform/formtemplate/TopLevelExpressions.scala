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
          case ReferenceInfo.SumExpr(_, Sum(FormCtx(FormComponentId(value)))) =>
            val exId = ExpressionId(value)
            if (expressionIdLookup(exId)) {
              List(expressionId ~> exId)
            } else {
              Nil
            }
          case _ => Nil
        }
    }

    layers.foldLeft(emptyGraph)(_ union Set(_))
  }

  def toTopologicalSort(
    graph: Graph[ExpressionId, GraphEdge.DiEdge]
  ): Either[graph.TopologicalSortFailure, Iterable[(Int, List[uk.gov.hmrc.gform.formtemplate.ExpressionId])]] = {

    def sortedOuterNodes(items: Iterable[graph.NodeT]) =
      items.toList
        .map(_.toOuter)

    graph.topologicalSort.map(_.toLayered.map { case (index, items) =>
      (index, sortedOuterNodes(items))
    })
  }

  def resolveReferences(exprSubstitutions: ExprSubstitutions): Either[UnexpectedState, ExprSubstitutions] = {

    val graph: Graph[ExpressionId, GraphEdge.DiEdge] = toGraph(exprSubstitutions)
    val selfReferencingExpressions =
      graph.edges.filter(edge => edge.source == edge.target).flatMap(_.value.map(_.value))

    if (selfReferencingExpressions.nonEmpty)
      Left(UnexpectedState(s"The expression ${selfReferencingExpressions.head.id} cannot reference itself"))
    else {
      val sort = toTopologicalSort(graph)

      sort.bimap(
        failure =>
          UnexpectedState(
            s"Cycle detected in top level expressions. Graph contains cycle: ${graph.findCycle}"
          ),
        iterable =>
          ExprSubstitutions(
            iterable.toList.reverse.foldLeft(exprSubstitutions.expressions) { case (acc0, (_, layerNodes)) =>
              layerNodes.foldLeft(acc0) { case (acc, expr) =>
                resolveExpr(acc, expr)
              }
            }
          )
      )
    }
  }

  private def resolveExpr(expressions: Map[ExpressionId, Expr], expressionId: ExpressionId): Map[ExpressionId, Expr] = {

    import uk.gov.hmrc.gform.formtemplate.ExprSubstituter._

    val substituter: Substituter[ExprSubstitutions, Expr] = implicitly[Substituter[ExprSubstitutions, Expr]]

    def loop(e: Expr): Expr = substituter.substitute(ExprSubstitutions(expressions), e)

    expressions.get(expressionId).fold(expressions) { expr =>
      expressions + (expressionId -> loop(expr))
    }
  }
}
