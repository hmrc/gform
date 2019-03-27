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

import cats.data.NonEmptyList
import org.scalatest.Assertion
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.NotChecked
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.structuredform.RoboticsXMLGeneratorSpec._
import uk.gov.hmrc.gform.structuredform.StructuredFormValue.{ ArrayNode, TextNode }

class StructuredFormDataBuilderSpec extends Spec {
  "apply(Form, FormTemplate)" must "create the correct JSON for simple fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createNonGroupField("field")
        )
      ),
      createForm(
        "field" -> "fieldValue"
      ),
      objectStructure(textField("field", "fieldValue"))
    )
  }

  it must "create the correct JSON for multivalue fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createMultiChoice("field")
        )
      ),
      createForm(
        "field" -> "value1,value2"
      ),
      objectStructure(arrayField("field", TextNode("value1"), TextNode("value2")))
    )
  }

  it must "create the correct JSON for MultiField fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createDate("field")
        )
      ),
      createForm(
        "field"       -> "",
        "field-day"   -> "1",
        "field-month" -> "2",
        "field-year"  -> "3"
      ),
      objectStructure(
        objectStructureField("field", textField("day", "1"), textField("month", "2"), textField("year", "3")))
    )
  }

  it must "create the correct JSON for simple fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createNonGroupField("field"))
        )
      ),
      createForm(
        "1_field" -> "fieldValue1",
        "2_field" -> "fieldValue2"
      ),
      objectStructure(arrayField("field", TextNode("fieldValue1"), TextNode("fieldValue2")))
    )
  }

  it must "create the correct JSON for multivalue fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createMultiChoice("field"))
        )
      ),
      createForm(
        "1_field" -> "value1,value2",
        "2_field" -> "value1,value3, value4"
      ),
      objectStructure(
        arrayField(
          "field",
          ArrayNode(List(TextNode("value1"), TextNode("value2"))),
          ArrayNode(List(TextNode("value1"), TextNode("value3"), TextNode("value4")))))
    )
  }

  it must "create the correct JSON for MultiField fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createDate("field"))
        )
      ),
      createForm(
        "field"         -> "",
        "1_field-day"   -> "1",
        "1_field-month" -> "2",
        "1_field-year"  -> "3",
        "2_field-day"   -> "4",
        "2_field-month" -> "5",
        "3_field-year"  -> "6"
      ),
      objectStructure(
        arrayField(
          "field",
          objectStructure(textField("day", "1"), textField("month", "2"), textField("year", "3")),
          objectStructure(textField("day", "4"), textField("month", "5"), textField("year", "6"))
        )
      )
    )

  }

  it must "create the correct JSON for simple fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createNonGroupField("field")
        )
      ),
      createForm(
        "field"   -> "fieldValue1",
        "2_field" -> "fieldValue2"
      ),
      objectStructure(arrayField("field", TextNode("fieldValue1"), TextNode("fieldValue2")))
    )
  }

  it must "create the correct JSON for multivalue fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createMultiChoice("field")
        )
      ),
      createForm(
        "field"   -> "value1,value2",
        "2_field" -> "value1,value3, value4"
      ),
      objectStructure(
        arrayField(
          "field",
          ArrayNode(List(TextNode("value1"), TextNode("value2"))),
          ArrayNode(List(TextNode("value1"), TextNode("value3"), TextNode("value4")))))
    )
  }

  it must "create the correct JSON for MultiField fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createDate("field")
        )
      ),
      createForm(
        "field"         -> "",
        "field-day"     -> "1",
        "field-month"   -> "2",
        "field-year"    -> "3",
        "2_field-day"   -> "4",
        "2_field-month" -> "5",
        "3_field-year"  -> "6"
      ),
      objectStructure(
        arrayField(
          "field",
          objectStructure(textField("day", "1"), textField("month", "2"), textField("year", "3")),
          objectStructure(textField("day", "4"), textField("month", "5"), textField("year", "6"))
        )
      )
    )
  }

  it must "include the acknowledgment and declaration sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createNonGroupField("field")
        ),
        Some(AcknowledgementSection(null, None, None, List(createNonGroupField("ackField")))),
        Some(DeclarationSection(null, None, None, List(createNonGroupField("decField"))))
      ),
      createForm(
        "field"    -> "fieldValue",
        "ackField" -> "ackFieldValue",
        "decField" -> "decFieldValue"
      ),
      objectStructure(
        textField("field", "fieldValue"),
        textField("ackField", "ackFieldValue"),
        textField("decField", "decFieldValue"))
    )
  }

  private def validate(
    formTemplate: FormTemplate,
    formData: Form,
    structuredFormValue: StructuredFormValue): Assertion =
    StructuredFormDataBuilder(formData, formTemplate) shouldBe
      structuredFormValue

  def createForm(fields: (String, String)*): Form =
    Form(
      FormId("TheForm"),
      null,
      null,
      FormTemplateId(""),
      FormData(fields.map { case (k, v) => FormField(FormComponentId(k), v) }),
      null,
      VisitIndex(Set(1)),
      ThirdPartyData.empty,
      None,
      NotChecked
    )

  def createFormTemplate(
    section: Section,
    acknowledgementSection: Option[AcknowledgementSection] = None,
    declarationSection: Option[DeclarationSection] = None): FormTemplate =
    FormTemplate(
      FormTemplateId(""),
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      List(section),
      acknowledgementSection.getOrElse(AcknowledgementSection("Ack", None, None, Nil)),
      declarationSection.getOrElse(DeclarationSection("Decl", None, None, Nil)),
      null
    )

  def createNonRepeatingSection(fields: FormComponent*): Section =
    Section(
      null,
      null,
      null,
      null,
      null,
      None,
      None,
      null,
      fields.toList,
      null,
      null
    )

  def createRepeatingSection(fields: FormComponent*): Section =
    Section(
      null,
      null,
      null,
      null,
      null,
      Some(TextExpression(Value)),
      Some(TextExpression(Value)),
      null,
      fields.toList,
      null,
      null
    )

  def createNonGroupField(id: String): FormComponent =
    createFormComponent(id, Text(AnyText, Value))

  def createGroup(fields: FormComponent*): FormComponent =
    createFormComponent("a group", Group(fields.toList, null))

  def createFormComponent(id: String, componentType: ComponentType): FormComponent =
    FormComponent(
      FormComponentId(id),
      componentType,
      "",
      null,
      null,
      null,
      true,
      true,
      true,
      true,
      true,
      null
    )

  def createMultiChoice(id: String): FormComponent =
    createFormComponent(id, Choice(Checkbox, NonEmptyList.of("One", "Two", "Three"), Vertical, Nil, None))

  def createRadio(id: String): FormComponent =
    createFormComponent(id, Choice(Radio, NonEmptyList.of("One", "Two", "Three"), Vertical, Nil, None))

  def createDate(id: String): FormComponent =
    createFormComponent(id, Date(AnyDate, Offset(0), None))
}
