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

package uk.gov.hmrc.gform.submission.ofsted

import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

import scala.xml.{ Atom, Node, Text }

object XmlToStructuredFormValue {
  def apply(node: Node): StructuredFormValue =
    (node.child.toList.partition(_.isInstanceOf[Text]) match {
      case (Nil, children) => buildObjectStructureFromChildren(children)
      case (texts, Nil)    => StructuredFormValue.TextNode(texts.mkString(""))
      case (_, children)   => buildObjectStructureFromChildren(children)
    })

  private def buildObjectStructureFromChildren(children: List[Node]): StructuredFormValue.ObjectStructure =
    buildObjectStructureFromGroupedChildren(children.groupBy(_.label))

  private def buildObjectStructureFromGroupedChildren(
    children: Map[String, List[Node]]): StructuredFormValue.ObjectStructure =
    StructuredFormValue.ObjectStructure(
      children.toList.map { case (n, vs) => createField(FieldName(n), vs) }
    )

  private def createField(name: FieldName, children: List[Node]): Field =
    Field(name, createArray(children))

  private def createArray(children: List[Node]): StructuredFormValue.ArrayNode =
    StructuredFormValue.ArrayNode(children.map(apply))
}
