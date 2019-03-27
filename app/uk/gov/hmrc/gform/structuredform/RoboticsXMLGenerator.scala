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

import scala.xml.{ NodeSeq, Text }

object RoboticsXMLGenerator {

  def apply(
    formId: FormId,
    dmsId: String,
    submissionReference: SubmissionRef,
    structuredForm: ObjectStructure): NodeSeq =
    <gform id = {formId.value} dms-id = {dmsId} submission-reference = {submissionReference.value}>{buildObjectStructureXml(structuredForm)}</gform>

  private def buildObjectStructureXml(value: ObjectStructure): NodeSeq =
    value.fields.flatMap(field => buildStructuredValueXml(field.name,field.value))

  def buildStructuredValueXml(fieldName: FieldName, value: StructuredFormValue): NodeSeq = value match {
    case TextNode(v)                      => <new>{v}</new>.copy(label = fieldName.name)
    case objectStructure: ObjectStructure => <new>{buildObjectStructureXml(objectStructure)}</new>.copy(label = fieldName.name)
    case arrayNode: ArrayNode             => <new>{buildArrayNodeXml(fieldName, arrayNode)}</new>.copy(label = fieldName.name + "s")

  }

  def buildArrayNodeXml(fieldName: FieldName, arrayNode: ArrayNode): NodeSeq = arrayNode.elements.flatMap {
    case TextNode(value)                  => <new>{value}</new>.copy(label = fieldName.name)
    case objectStructure: ObjectStructure => <new>{buildObjectStructureXml(objectStructure)}</new>.copy(label = fieldName.name)
    case array: ArrayNode                 => <new>{buildArrayNodeXml(fieldName, array)}</new>.copy(label = fieldName.name + "s")
  }

  def generateLabel(field: Field): FieldName = {
    val fieldName = field.name.name

    field.value match {
      case _: ArrayNode => FieldName(fieldName + "s")
      case _            => FieldName(fieldName)
    }
  }

}
