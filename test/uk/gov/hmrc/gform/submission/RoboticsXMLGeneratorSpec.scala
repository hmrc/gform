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

import java.time.{ Instant, LocalDate, ZoneId }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue._
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

import scala.xml.{ Elem, NodeSeq, Text }

class RoboticsXMLGeneratorSpec extends Spec {
  import RoboticsXMLGeneratorSpec._

  implicit class FieldNameSyntax(fieldName: String) {
    def ~(value: String) = Field(FieldName(fieldName), TextNode(value))
    def ~(fields: Field*) = Field(FieldName(fieldName), objectStructure(fields: _*))
    def ~(value: StructuredFormValue) = Field(FieldName(fieldName), value)
  }

  "apply" should "generate the correct XML for an empty object structure" in {

    val objectStructure = ObjectStructure(Nil)

    verifyXml(objectStructure, NodeSeq.Empty)
  }

  it should "generate the correct XML for an object structure with simple fields" in {
    verifyXml(objectStructure("field1" ~ "value1", "field2" ~ "value2"), <field1>value1</field1><field2>value2</field2>)
  }

  it should "generate the correct XML for an object structure with a field that is also an object structure" in {
    verifyXml(
      objectStructure("objectStructureField" ~ ("field" ~ "value")),
      <objectStructureField><field>value</field></objectStructureField>
    )
  }

  it should "generate the correct XML for an object structure with a field that is an array" in {

    verifyXml(
      objectStructure(arrayField("arrayField", TextNode("value1"), TextNode("value2"))),
      <arrayFields><arrayField seqNum ="0">value1</arrayField><arrayField seqNum ="1">value2</arrayField></arrayFields>
    )

  }

  it should "generate the correct XML for an object structure with a field that is an array that contains objects" in {

    verifyXml(
      objectStructure(
        arrayField(
          "arrayField",
          objectStructure(
            objectStructureField(
              "objectStructureField",
              textField("txtfield1", "txtval1"),
              textField("txtfield2", "txtval2")
            )
          ),
          TextNode("value2")
        )
      ),
      <arrayFields>
        <arrayField seqNum ="0">
          <objectStructureField>
          <txtfield1>txtval1</txtfield1>
          <txtfield2>txtval2</txtfield2>
          </objectStructureField>
        </arrayField>
      <arrayField seqNum ="1">value2</arrayField>
      </arrayFields>
    )

  }

  it should "generate the correct XML for an object structure that occurs when there are MultiField fields in repeating sections" in {

    verifyXml(
      objectStructure(
        arrayField(
          "field",
          objectStructure(textField("day", "1"), textField("month", "2"), textField("year", "3")),
          objectStructure(textField("day", "4"), textField("month", "5"), textField("year", "6"))
        )
      ),
      <fields>
        <field seqNum ="0">
          <day>1</day>
          <month>2</month>
          <year>3</year>
        </field>
        <field seqNum ="1">
          <day>4</day>
          <month>5</month>
          <year>6</year>
        </field>
      </fields>
    )

  }

  "apply (for handlebars)" should "generate the correct XML for an empty handlebar" in {
    val emptyHandlebarFormData: String =
      <formData>
      </formData>
        .toString()

    verifyHandlebarXml(emptyHandlebarFormData)
  }

  it should "generate the correct XML for a simple handlebar" in {
    val simpleHandlebarFormData: String =
      <formData>
        <user>
          <name>John Johnson</name>
          <telephone>07777777777</telephone>
          <email>user@test.com</email>
        </user>
      </formData>
        .toString()

    verifyHandlebarXml(simpleHandlebarFormData)
  }

  it should "generate the correct XML for handlebars with more than one nesting" in {
    val complexHandlebarFormData: String =
      <formData>
        <claim>
          <taxYear>2024</taxYear>
          <nilReturn>false</nilReturn>
          <england>
            <taxDue>1.00</taxDue>
            <niDue>2.00</niDue>
          </england>
          <scotland>
            <taxDue>3.00</taxDue>
            <niDue>4.00</niDue>
          </scotland>
          <wales>
            <taxDue>5.00</taxDue>
            <niDue>6.00</niDue>
          </wales>
          <totalTaxDue>9.00</totalTaxDue>
          <totalNiDue>12.00</totalNiDue>
          <totalOverallDue>21.00</totalOverallDue>
        </claim>
      </formData>
        .toString()

    verifyHandlebarXml(complexHandlebarFormData)
  }

  "buildDataStoreXML" should "generate the correct XML for the data-store with a field that is also an object structure" in {

    RoboticsXMLGenerator.buildDataStoreXML(
      objectStructure(arrayField("arrayField", TextNode("value1"), TextNode("value2"))),
      false
    ) shouldBe <gform><arrayFields><arrayField seqNum="0">value1</arrayField><arrayField seqNum="1">value2</arrayField></arrayFields></gform>

  }
}
case object RoboticsXMLGeneratorSpec extends Spec {

  private def verifyXml(objectStructure: ObjectStructure, expectedFields: NodeSeq) = {

    val formId = FormTemplateId("formId")
    val dmsId = "dmsId"
    val submissionRef = SubmissionRef("submissionRef")
    val envelopeId = EnvelopeId("envelopeId")
    val lEn = LangADT.En
    val lCy = LangADT.Cy

    val expected: Elem =
      <gform id={formId.value} dms-id={dmsId} submission-reference={submissionRef.value}>{
        removeWhitespace(expectedFields)
      }<dateSubmitted>02/01/2019</dateSubmitted><datetimeSubmitted>2019-01-02T00:00:00Z</datetimeSubmitted><userLanguage>EN</userLanguage><correlationId>{
        envelopeId.value
      }</correlationId></gform>

    val dateSubmitted = Instant.from(LocalDate.of(2019, 1, 2).atStartOfDay(ZoneId.of("Europe/London")))

    val dateSubmittedBST = Instant.from(LocalDate.of(2019, 6, 1).atStartOfDay(ZoneId.of("Europe/London")))

    val expectedBST: Elem =
      <gform id={formId.value} dms-id={dmsId} submission-reference={submissionRef.value}>{
        removeWhitespace(expectedFields)
      }<dateSubmitted>01/06/2019</dateSubmitted><datetimeSubmitted>2019-06-01T00:00:00+01:00</datetimeSubmitted><userLanguage>CY</userLanguage><correlationId>{
        envelopeId.value
      }</correlationId></gform>

    RoboticsXMLGenerator
      .apply(
        formId,
        dmsId,
        submissionRef,
        objectStructure,
        dateSubmitted,
        lEn,
        Some(envelopeId),
        sanitizeRequired = false
      ) shouldBe expected
    RoboticsXMLGenerator
      .apply(
        formId,
        dmsId,
        submissionRef,
        objectStructure,
        dateSubmittedBST,
        lCy,
        Some(envelopeId),
        sanitizeRequired = false
      ) shouldBe expectedBST
  }

  private def verifyHandlebarXml(formData: String) = {

    val formId = FormTemplateId("formId")
    val dmsId = "dmsId"
    val submissionRef = SubmissionRef("submissionRef")
    val envelopeId = EnvelopeId("envelopeId")
    val lEn = LangADT.En
    val lCy = LangADT.Cy

    val expected: Elem =
      <gform id={formId.value} dms-id={dmsId} submission-reference={
        submissionRef.value
      }><dateSubmitted>02/01/2019</dateSubmitted><datetimeSubmitted>2019-01-02T00:00:00Z</datetimeSubmitted><userLanguage>EN</userLanguage><correlationId>{
        envelopeId.value
      }</correlationId>{
        xml.XML.loadStringNodes(formData)
      }</gform>

    val dateSubmitted = Instant.from(LocalDate.of(2019, 1, 2).atStartOfDay(ZoneId.of("Europe/London")))

    val dateSubmittedBST = Instant.from(LocalDate.of(2019, 6, 1).atStartOfDay(ZoneId.of("Europe/London")))

    val expectedBST: Elem =
      <gform id={formId.value} dms-id={dmsId} submission-reference={
        submissionRef.value
      }><dateSubmitted>01/06/2019</dateSubmitted><datetimeSubmitted>2019-06-01T00:00:00+01:00</datetimeSubmitted><userLanguage>CY</userLanguage><correlationId>{
        envelopeId.value
      }</correlationId>{
        xml.XML.loadStringNodes(formData)
      }</gform>

    RoboticsXMLGenerator
      .apply(
        formId,
        dmsId,
        submissionRef,
        dateSubmitted,
        lEn,
        Some(envelopeId),
        formData
      ) shouldBe expected
    RoboticsXMLGenerator
      .apply(
        formId,
        dmsId,
        submissionRef,
        dateSubmittedBST,
        lCy,
        Some(envelopeId),
        formData
      ) shouldBe expectedBST
  }

  def removeWhitespace(ns: NodeSeq): NodeSeq = ns.map {
    case txt: Text => txt
    case node      => scala.xml.Utility.trim(node)
  }

  def textField(fieldName: String, value: String): Field = Field(FieldName(fieldName), TextNode(value))

  def objectStructureField(fieldName: String, fields: Field*): Field =
    Field(FieldName(fieldName), ObjectStructure(fields.toList))

  def objectStructure(fields: Field*): StructuredFormValue.ObjectStructure = ObjectStructure(fields.toList)

  def arrayField(fieldName: String, fields: StructuredFormValue*) =
    Field(FieldName(fieldName), ArrayNode(fields.toList))

}
