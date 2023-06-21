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

package uk.gov.hmrc.gform.submission
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import java.time.{ Instant, ZoneId }
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SubmissionRef }
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
    now: Instant,
    l: LangADT,
    maybeEnvelopeId: Option[EnvelopeId],
    sanitizeRequired: Boolean
  ): NodeSeq =
    <gform id={formId.value} dms-id={dmsId} submission-reference={submissionReference.withoutHyphens}>{
      buildObjectStructureXml(structuredForm, sanitizeRequired)
    }{dateSubmitted(now)}{datetimeSubmitted(now)}{userLanguage(l)}{
      maybeEnvelopeId.map(correlationId).getOrElse("")
    }</gform>

  private val replacements =
    Map('<' -> "&lt;", '>' -> "&gt;", '&' -> "&#38;", ']' -> "&#93;", '\'' -> "&#39;", '"' -> "&#34;")

  private val dateSubmittedFormater = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.of("Europe/London"))

  private def dateSubmitted(now: Instant): NodeSeq =
    <dateSubmitted>{dateSubmittedFormater.format(now)}</dateSubmitted>

  private val datetimeSubmittedFormater = ISO_OFFSET_DATE_TIME

  private def datetimeSubmitted(now: Instant): NodeSeq =
    <datetimeSubmitted>{datetimeSubmittedFormater.format(now.atZone(ZoneId.of("Europe/London")))}</datetimeSubmitted>

  private def buildStructuredValueXml(
    field: Field,
    value: StructuredFormValue,
    sanitizeRequired: Boolean,
    index: Option[Int] = None
  ): NodeSeq =
    value match {
      case TextNode(content)                => textNodeTag(content, field, sanitizeRequired, index)
      case objectStructure: ObjectStructure => objectStructureTag(objectStructure, field, sanitizeRequired, index)
      case arrayNode: ArrayNode             => arrayNodeTag(arrayNode, field, sanitizeRequired, index)
    }

  private def buildObjectStructureXml(value: ObjectStructure, sanitizeRequired: Boolean): NodeSeq =
    value.fields.flatMap(field => buildStructuredValueXml(field, field.value, sanitizeRequired))

  private def buildArrayNodeXml(field: Field, arrayNode: ArrayNode, sanitizeRequired: Boolean): NodeSeq =
    arrayNode.elements.zipWithIndex.flatMap { case (structuredFormValue: StructuredFormValue, index: Int) =>
      buildStructuredValueXml(field, structuredFormValue, sanitizeRequired, Some(index))
    }

  private def textNodeTag(content: String, field: Field, sanitizeRequired: Boolean, index: Option[Int]): Elem =
    <new>{if (sanitizeRequired) sanitizeContent(content) else content}</new>
      .copy(label = getRoboticsXmlName(field), attributes = getAttribute(index))

  private def getRoboticsXmlName(field: Field) =
    field.nameFor(RoboticsXml).name

  private def objectStructureTag(
    objectStructure: ObjectStructure,
    field: Field,
    sanitizeRequired: Boolean,
    index: Option[Int]
  ): Elem =
    <new>{buildObjectStructureXml(objectStructure, sanitizeRequired)}</new>
      .copy(label = getRoboticsXmlName(field), attributes = getAttribute(index))

  private def arrayNodeTag(arrayNode: ArrayNode, field: Field, sanitizeRequired: Boolean, index: Option[Int]): Elem =
    <new>{buildArrayNodeXml(field, arrayNode, sanitizeRequired)}</new>
      .copy(label = getRoboticsXmlName(field) + "s", attributes = getAttribute(index))

  private def getAttribute(index: Option[Int]) = index match {
    case Some(i) => <new seqNum ={i.toString}></new>.attributes
    case None    => <new></new>.attributes
  }

  private def userLanguage(l: LangADT) = <userLanguage>{l.langADTToString.toUpperCase}</userLanguage>

  private def correlationId(envelopeId: EnvelopeId) = <correlationId>{envelopeId.value}</correlationId>

  private def sanitizeContent(content: String): String =
    content.foldLeft("") { (acc, char) =>
      replacements.getOrElse(char, char.toString) match {
        case replacement: String => acc + replacement
        case _                   => acc
      }
    }

}
