/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object DependencyGraph {

  private val emptyGraph: Graph[FormComponentId, DiEdge] = Graph.empty

  def toGraph(formTemplate: FormTemplate): Graph[FormComponentId, DiEdge] = {
    val graphs: List[Graph[FormComponentId, DiEdge]] = formTemplate.sections.flatMap(_.fields.map(fromFormComponent))
    graphs.foldLeft(emptyGraph)(_ ++ _)
  }

  def constructDepencyGraph(
    graph: Graph[FormComponentId, DiEdge]): Either[graph.NodeT, graph.LayeredTopologicalOrder[graph.NodeT]] =
    graph.topologicalSort.map(_.toLayered)

  private def fromFormComponent(fc: FormComponent): Graph[FormComponentId, DiEdge] = {
    val fcIds: List[FormComponentId] = fc match {
      case HasExpr(expr) => fromExpr(expr)
      case _             => List.empty
    }

    fcIds.map(fc.id ~> _).foldLeft(emptyGraph)(_ + _)
  }

  private def fromExpr(expr: Expr): List[FormComponentId] =
    expr match {
      case FormCtx(fc) => FormComponentId(fc) :: Nil
      // case Sum(FormCtx(fc)) => ??? // TODO JoVl implement once GFC-544 is fixed
      case Add(field1, field2)         => fromExpr(field1) ++ fromExpr(field2)
      case Subtraction(field1, field2) => fromExpr(field1) ++ fromExpr(field2)
      case Multiply(field1, field2)    => fromExpr(field1) ++ fromExpr(field2)
      case otherwise                   => List.empty
    }
}
