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

package uk.gov.hmrc.gform.structuredform
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.submission.SubmissionRef

import scala.xml.{ Elem, NodeSeq, Text }

object RoboticsXMLGenerator {

  def apply(
    formId: FormId,
    dmsId: String,
    submissionReference: SubmissionRef,
    structuredForm: ObjectStructure): NodeSeq =
    <gform id = {formId.value} dms-id = {dmsId} submission-reference = {submissionReference.value}>{buildObjectStructureXml(structuredForm)}</gform>

  private def buildObjectStructureXml(value: ObjectStructure): NodeSeq =
    value.fields.flatMap(field => buildStructuredValueXml(field.name, field.value))

  def buildStructuredValueXml(fieldName: FieldName, value: StructuredFormValue): NodeSeq = value match {
    case TextNode(content)                => textNodeTag(content, fieldName)
    case objectStructure: ObjectStructure => objectStructureTag(objectStructure, fieldName)
    case arrayNode: ArrayNode             => arrayNodeTag(arrayNode, fieldName)
  }

  def buildArrayNodeXml(name: FieldName, arrayNode: ArrayNode): NodeSeq = arrayNode.elements.flatMap {
    buildStructuredValueXml(name, _)
  }

  private def textNodeTag(content: String, fieldName: FieldName): Elem =
    <new>{content}</new>.copy(label = fieldName.name)

  private def objectStructureTag(objectStructure: ObjectStructure, fieldName: FieldName): Elem =
    <new>{buildObjectStructureXml(objectStructure)}</new>.copy(label = fieldName.name)

  private def arrayNodeTag(arrayNode: ArrayNode, fieldName: FieldName): Elem =
    <new>{buildArrayNodeXml(fieldName, arrayNode)}</new>.copy(label = fieldName.name + "s")

}
