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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.syntax.either._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object DependencyGraph {

  val emptyGraph: Graph[FormComponentId, DiEdge] = Graph.empty

  def toGraph(formTemplate: FormTemplate): Graph[FormComponentId, DiEdge] =
    graphFrom(formTemplate.expandedFormComponentsInMainSections)

  private def graphFrom(expandedFormComponents: List[FormComponent]): Graph[FormComponentId, DiEdge] = {

    val allFcIds = expandedFormComponents.map(_.id)

    def fromFormComponent(fc: FormComponent): Graph[FormComponentId, DiEdge] = {
      def fcIds(fc: FormComponent): List[FormComponentId] = fc match {
        case HasExpr(SingleExpr(expr)) => eval(expr)
        case _                         => List.empty
      }
      fcIds(fc).map(fc.id ~> _).foldLeft(emptyGraph)(_ union Set(_))
    }

    def eval(expr: Expr): List[FormComponentId] =
      expr match {
        case FormCtx(fcId)               => fcId :: Nil
        case Sum(FormCtx(fc))            => allFcIds.filter(_.value.endsWith(fc.value))
        case Add(field1, field2)         => eval(field1) ++ eval(field2)
        case Subtraction(field1, field2) => eval(field1) ++ eval(field2)
        case Multiply(field1, field2)    => eval(field1) ++ eval(field2)
        case Divide(field1, field2)      => eval(field1) ++ eval(field2)
        case otherwise                   => List.empty
      }

    expandedFormComponents.foldLeft(emptyGraph)(_ ++ fromFormComponent(_))
  }

  def constructDependencyGraph(
    graph: Graph[FormComponentId, DiEdge]
  ): Either[graph.TopologicalSortFailure, graph.LayeredTopologicalOrder[graph.NodeT]] =
    graph.topologicalSort.map(_.toLayered)
}
