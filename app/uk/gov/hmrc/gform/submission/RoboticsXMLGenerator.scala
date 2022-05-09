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

package uk.gov.hmrc.gform.submission
import java.time.{ Instant, ZoneId }
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_INSTANT
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, RoboticsXml, StructuredFormValue }

import scala.xml.{ Elem, NodeSeq }

object RoboticsXMLGenerator {

  def apply(
    formId: FormTemplateId,
    dmsId: String,
    submissionReference: SubmissionRef,
    structuredForm: ObjectStructure,
    now: Instant
  ): NodeSeq =
    <gform id={formId.value} dms-id={dmsId} submission-reference={submissionReference.withoutHyphens}>{
      buildObjectStructureXml(structuredForm)
    }{dateSubmitted(now)}{datetimeSubmitted(now)}</gform>

  private val dateSubmittedFormater = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.of("Europe/London"))

  private def dateSubmitted(now: Instant): NodeSeq =
    <dateSubmitted>{dateSubmittedFormater.format(now)}</dateSubmitted>

  private val datetimeSubmittedFormater = ISO_INSTANT

  private def datetimeSubmitted(now: Instant): NodeSeq =
    <datetimeSubmitted>{datetimeSubmittedFormater.format(now)}</datetimeSubmitted>

  private def buildStructuredValueXml(field: Field, value: StructuredFormValue, index: Option[Int] = None): NodeSeq =
    value match {
      case TextNode(content)                => textNodeTag(content, field, index)
      case objectStructure: ObjectStructure => objectStructureTag(objectStructure, field, index)
      case arrayNode: ArrayNode             => arrayNodeTag(arrayNode, field, index)
    }

  private def buildObjectStructureXml(value: ObjectStructure): NodeSeq =
    value.fields.flatMap(field => buildStructuredValueXml(field, field.value))

  private def buildArrayNodeXml(field: Field, arrayNode: ArrayNode): NodeSeq =
    arrayNode.elements.zipWithIndex.flatMap { case (structuredFormValue: StructuredFormValue, index: Int) =>
      buildStructuredValueXml(field, structuredFormValue, Some(index))
    }

  private def textNodeTag(content: String, field: Field, index: Option[Int]): Elem =
    <new>{content}</new>.copy(label = getRoboticsXmlName(field), attributes = getAttribute(index))

  private def getRoboticsXmlName(field: Field) =
    field.nameFor(RoboticsXml).name

  private def objectStructureTag(objectStructure: ObjectStructure, field: Field, index: Option[Int]): Elem =
    <new>{buildObjectStructureXml(objectStructure)}</new>
      .copy(label = getRoboticsXmlName(field), attributes = getAttribute(index))

  private def arrayNodeTag(arrayNode: ArrayNode, field: Field, index: Option[Int]): Elem =
    <new>{buildArrayNodeXml(field, arrayNode)}</new>
      .copy(label = getRoboticsXmlName(field) + "s", attributes = getAttribute(index))

  private def getAttribute(index: Option[Int]) = index match {
    case Some(i) => <new seqNum ={i.toString}></new>.attributes
    case None    => <new></new>.attributes
  }

}
