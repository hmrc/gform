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
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.submission.SubmissionRef

import scala.xml.{ Elem, NodeSeq, Text }

class RoboticsXMLGeneratorSpec extends Spec {
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
      <objectStructureField><field>value</field></objectStructureField>)
  }

  it should "generate the correct XML for an object structure with a field that is an array" in {

    verifyXml(
      objectStructure(arrayField("arrayField", TextNode("value1"), TextNode("value2"))),
      <arrayFields><arrayField>value1</arrayField><arrayField>value2</arrayField></arrayFields>
    )

  }

  private def verifyXml(objectStructure: ObjectStructure, expectedFields: NodeSeq) = {

    val formId = FormId("formId")
    val dmsId = "dmsId"
    val submissionRef = SubmissionRef("submissionRef")

    val expected: Elem =
      <gform id = {formId.value} dms-id = {dmsId} submission-reference = {submissionRef.value}>{removeWhitespace(expectedFields)}</gform>

    RoboticsXMLGenerator.apply(formId, dmsId, submissionRef, objectStructure) shouldBe expected
  }

  private def removeWhitespace(ns: NodeSeq): NodeSeq = ns.map {
    case txt: Text => txt
    case node      => scala.xml.Utility.trim(node)
  }

  private def textField(fieldName: String, value: String): Field = Field(FieldName(fieldName), TextNode(value))

  private def objectStructureField(fieldName: String, fields: Field*): Field =
    Field(FieldName(fieldName), ObjectStructure(fields.toList))

  private def objectStructure(fields: Field*): StructuredFormValue.ObjectStructure = ObjectStructure(fields.toList)

  private def arrayField(fieldName: String, fields: StructuredFormValue*) =
    Field(FieldName(fieldName), ArrayNode(fields.toList))
}
