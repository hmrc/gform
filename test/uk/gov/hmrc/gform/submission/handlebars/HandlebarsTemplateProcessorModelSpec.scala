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

package uk.gov.hmrc.gform.submission.handlebars

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.NotChecked
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class HandlebarsTemplateProcessorModelSpec extends Spec {
  "apply(Form, FormTemplate)" must "create the correct JSON" in {
    val formTemplate =
      createFormTemplate(
        createNonRepeatingSection(
          createNonGroupField("nonRepeatingSection1Field"),
          createGroupField(
            "group",
            createNonGroupField("groupField1"),
            createNonGroupField("groupField2")
          )
        ),
        createRepeatingSection(
          createNonGroupField("field1InRepeatingSection"),
          createNonGroupField("field2InRepeatingSection")
        )
      )

    val form = createForm(
      "nonRepeatingSection1Field"  -> "nonRepeatingSection1FieldValue",
      "groupField1"                -> "groupField1Value1",
      "1_groupField1"              -> "groupField1Value2",
      "groupField2"                -> "groupField2Value1",
      "1_field1InRepeatingSection" -> "field1InRepeatingSectionValue1",
      "2_field1InRepeatingSection" -> "field1InRepeatingSectionValue2",
      "1_field2InRepeatingSection" -> "field2InRepeatingSectionValue1",
      "2_field2InRepeatingSection" -> "field2InRepeatingSectionValue2"
    )

    HandlebarsTemplateProcessorModel(form, formTemplate) shouldBe
      HandlebarsTemplateProcessorModel(
        """|{
           |  "nonRepeatingSection1Field" : "nonRepeatingSection1FieldValue",
           |  "groupField1" : [
           |    "groupField1Value1",
           |    "groupField1Value2"
           |  ],
           |  "groupField2" : [
           |    "groupField2Value1"
           |  ],
           |  "field1InRepeatingSection" : [
           |    "field1InRepeatingSectionValue1",
           |    "field1InRepeatingSectionValue2"
           |  ],
           |  "field2InRepeatingSection" : [
           |    "field2InRepeatingSectionValue1",
           |    "field2InRepeatingSectionValue2"
           |  ]
           |}""".stripMargin
      )
  }

  "+" must "shallow merge the two models" in {
    HandlebarsTemplateProcessorModel("""{ "a": 1 }""") + HandlebarsTemplateProcessorModel("""{ "b": 2 }""") shouldBe
      HandlebarsTemplateProcessorModel("""{ "a": 1, "b": 2 }""")
  }

  def createForm(fields: (String, String)*): Form =
    Form(
      null,
      null,
      null,
      FormTemplateId(""),
      FormData(fields.map { case (k, v) => FormField(FormComponentId(k), v) }),
      null,
      VisitIndex(Set(1)),
      None,
      NotChecked)

  def createFormTemplate(sections: Section*): FormTemplate =
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
      sections.toList,
      null,
      null,
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

  def createGroupField(id: String, fields: FormComponent*): FormComponent =
    createFormComponent(id, Group(fields.toList, null))

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
}
