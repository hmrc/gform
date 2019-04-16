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

package uk.gov.hmrc.gform.submission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ FieldName, StructuredFormValue }

import scala.xml.{ Elem, NodeSeq }

object RoboticsXMLGenerator {

  def apply(
    formId: FormTemplateId,
    dmsId: String,
    submissionReference: SubmissionRef,
    structuredForm: ObjectStructure): NodeSeq =
    <gform id={formId.value} dms-id={dmsId} submission-reference={submissionReference.value}>{buildObjectStructureXml(structuredForm)}</gform>

  private def buildStructuredValueXml(
    fieldName: FieldName,
    value: StructuredFormValue,
    index: Option[Int] = None): NodeSeq = value match {
    case TextNode(content)                => textNodeTag(content, fieldName, index)
    case objectStructure: ObjectStructure => objectStructureTag(objectStructure, fieldName, index)
    case arrayNode: ArrayNode             => arrayNodeTag(arrayNode, fieldName, index)
  }

  private def buildObjectStructureXml(value: ObjectStructure): NodeSeq =
    value.fields.flatMap(field => buildStructuredValueXml(field.name, field.value))

  private def buildArrayNodeXml(name: FieldName, arrayNode: ArrayNode): NodeSeq =
    arrayNode.elements.zipWithIndex.flatMap {
      case (structuredFormValue: StructuredFormValue, index: Int) =>
        buildStructuredValueXml(name, structuredFormValue, Some(index))
    }

  private def textNodeTag(content: String, fieldName: FieldName, index: Option[Int]): Elem =
    <new>{content}</new>.copy(label = fieldName.name, attributes = getAttribute(index))

  private def objectStructureTag(objectStructure: ObjectStructure, fieldName: FieldName, index: Option[Int]): Elem =
    <new>{buildObjectStructureXml(objectStructure)}</new>.copy(label = fieldName.name, attributes = getAttribute(index))

  private def arrayNodeTag(arrayNode: ArrayNode, fieldName: FieldName, index: Option[Int]): Elem =
    <new>{buildArrayNodeXml(fieldName, arrayNode)}</new>
      .copy(label = fieldName.name + "s", attributes = getAttribute(index))

  private def getAttribute(index: Option[Int]) = index match {
    case Some(i) => <new seqNum ={i.toString}></new>.attributes
    case None    => <new></new>.attributes
  }

}
