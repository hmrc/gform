/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.services

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models._

import scala.collection.immutable.List

class RepeatingComponentServiceSpec extends Spec {

  val basicFormTemplate = FormTemplate(
    schemaId = None,
    formTypeId = FormTypeId("JustAFormTypeId"),
    formName = "formName",
    version = Version("-11"),
    description = "formTemplateDescription",
    characterSet = "UTF-16",
    dmsSubmission = DmsSubmission("customerId", "classificationType", "businessArea"),
    authConfig = AuthConfig(AuthModule("TEST"), None, RegimeId("TEST")),
    submitSuccessUrl = "http://somewhere-nice.net",
    submitErrorUrl = "http://somewhere-nasty.net",
    sections = Nil
  )

  val testService = RepeatingComponentService

  val mFormData = FormData(
    userId = UserId("userId"),
    formTypeId = FormTypeId("formTypeId"),
    version = Version("-11"),
    characterSet = "EBCDIC",
    fields = Seq.empty
  )

  val form = Form(
    _id = FormId("forma"),
    formData = mFormData,
    envelopeId = EnvelopeId("envId")
  )

  "getAllSections" should "return only sections in template when no repeating sections are defined" in {

    val textFieldUno = FieldValue(
      id = FieldId("UNO"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val group = Group(
      fields = List(textFieldUno),
      orientation = Horizontal,
      repeatsMax = None,
      repeatsMin = None,
      repeatLabel = None,
      repeatAddAnotherText = None
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true,
      errorMessage = None
    )

    val section = Section(
      title = "Section title",
      description = None,
      shortName = None,
      includeIf = None,
      None, None, None,
      fields = List(groupFieldValue)
    )
    val formTemplate = basicFormTemplate.copy(sections = List(section))

    testService.getAllSections(form, formTemplate) shouldBe List(section)
  }

  it should "return no dynamically created sections when fieldToTrack in repeating group and no form data" in {

    val textFieldUno = FieldValue(
      id = FieldId("repeatingSectionDriver"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val group = Group(
      fields = List(textFieldUno),
      orientation = Horizontal,
      repeatsMax = Some(4),
      repeatsMin = Some(1),
      repeatLabel = Some("RepGrpLabel"),
      repeatAddAnotherText = Some("AddButtonLabel")
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true,
      errorMessage = None
    )

    val section1 = Section(
      title = "Section 1 title",
      description = None,
      shortName = None,
      includeIf = None,
      repeatsMax = None,
      repeatsMin = None,
      fieldToTrack = None,
      fields = List(groupFieldValue)
    )

    val textFieldDos = FieldValue(
      id = FieldId("DOS"),
      `type` = Text(AnyText, Constant("DOS"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val section2 = Section(
      title = "Repeating section title",
      description = None,
      shortName = None,
      includeIf = None,
      repeatsMax = Some(TextExpression(Constant("2"))),
      repeatsMin = Some(TextExpression(Constant("1"))),
      fieldToTrack = Some(VariableInContext("repeatingSectionDriver")),
      fields = List(textFieldDos)
    )
    val formTemplate = basicFormTemplate.copy(sections = List(section1, section2))

    val expectedList = List(section1)

    testService.getAllSections(form, formTemplate) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when fieldToTrack in repeating group, and non-empty form data" in {

    val textFieldUno = FieldValue(
      id = FieldId("repeatingSectionDriver"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val group = Group(
      fields = List(textFieldUno),
      orientation = Horizontal,
      repeatsMax = Some(4),
      repeatsMin = Some(1),
      repeatLabel = Some("RepGrpLabel"),
      repeatAddAnotherText = Some("AddButtonLabel")
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true,
      errorMessage = None
    )

    val section1 = Section(
      title = "Section 1 title",
      description = None,
      shortName = None,
      includeIf = None,
      repeatsMax = None,
      repeatsMin = None,
      fieldToTrack = None,
      fields = List(groupFieldValue)
    )

    val textFieldDos = FieldValue(
      id = FieldId("DOS"),
      `type` = Text(AnyText, Constant("DOS"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val section2 = Section(
      title = "$t, $n",
      description = None,
      shortName = Some("$n, $t"),
      includeIf = None,
      repeatsMax = Some(TextExpression(Constant("2"))),
      repeatsMin = Some(TextExpression(Constant("1"))),
      fieldToTrack = Some(VariableInContext("repeatingSectionDriver")),
      fields = List(textFieldDos)
    )
    val formTemplate = basicFormTemplate.copy(sections = List(section1, section2))

    val textFieldR = textFieldDos.copy(id = FieldId(s"1_${textFieldDos.id.value}"))
    val sectionR = section2.copy(fields = List(textFieldR), title = "ONE, 1", shortName = Some("1, ONE"))

    val textFieldR2 = textFieldDos.copy(id = FieldId(s"2_${textFieldDos.id.value}"))
    val sectionR2 = section2.copy(fields = List(textFieldR2), title = "TWO, 2", shortName = Some("2, TWO"))

    val expectedList = List(section1, sectionR, sectionR2)

    val newFormData = mFormData.copy(fields = Seq(
      FormField(FieldId("repeatingSectionDriver"), "ONE"),
      FormField(FieldId("1_repeatingSectionDriver"), "TWO"),
      FormField(FieldId("1_DOS"), "ONE"),
      FormField(FieldId("2_DOS"), "TWO")
    ))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }

  it should "return a dynamically created section when fieldToTrack in a NON-repeating group)" in {

    val textFieldUno = FieldValue(
      id = FieldId("repeatingSectionDriver"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val group = Group(
      fields = List(textFieldUno),
      orientation = Horizontal,
      repeatsMax = None,
      repeatsMin = None,
      repeatLabel = None,
      repeatAddAnotherText = None
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true,
      errorMessage = None
    )

    val section1 = Section(
      title = "Section 1 title",
      description = None,
      shortName = None,
      includeIf = None,
      repeatsMax = None,
      repeatsMin = None,
      fieldToTrack = None,
      fields = List(groupFieldValue)
    )

    val textFieldDos = FieldValue(
      id = FieldId("DOS"),
      `type` = Text(AnyText, Constant("DOS"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val section2 = Section(
      title = "Repeating section title $n",
      description = None,
      shortName = Some("shortName $n"),
      includeIf = None,
      repeatsMax = Some(TextExpression(Constant("2"))),
      repeatsMin = Some(TextExpression(Constant("1"))),
      fieldToTrack = Some(VariableInContext("repeatingSectionDriver")),
      fields = List(textFieldDos)
    )
    val formTemplate = basicFormTemplate.copy(sections = List(section1, section2))

    val textFieldDosR = textFieldDos.copy(id = FieldId(s"1_${textFieldDos.id.value}"))
    val sectionR = section2.copy(fields = List(textFieldDosR), title = "Repeating section title 1", shortName = Some("shortName 1"))
    val expectedList = List(section1, sectionR)

    val newFormData = mFormData.copy(fields = Seq(
      FormField(FieldId("1_DOS"), "EEITT-866")
    ))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when fieldToTrack in a NON-repeating group, with form data" in {

    val textFieldUno = FieldValue(
      id = FieldId("repeatingSectionDriver"),
      `type` = Text(AnyText, Constant("UNO"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val group = Group(
      fields = List(textFieldUno),
      orientation = Horizontal,
      repeatsMax = None,
      repeatsMin = None,
      repeatLabel = None,
      repeatAddAnotherText = None
    )

    val groupFieldValue = FieldValue(
      id = FieldId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = false,
      submissible = true,
      errorMessage = None
    )

    val section1 = Section(
      title = "Section 1 title",
      description = None,
      shortName = None,
      includeIf = None,
      repeatsMax = None,
      repeatsMin = None,
      fieldToTrack = None,
      fields = List(groupFieldValue)
    )

    val textFieldDos = FieldValue(
      id = FieldId("DOS"),
      `type` = Text(AnyText, Constant("DOS"), false),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      mandatory = true,
      editable = true,
      submissible = true,
      errorMessage = None
    )

    val section2 = Section(
      title = "Repeating section title $n",
      description = None,
      shortName = Some("shortName $n"),
      includeIf = None,
      repeatsMax = Some(TextExpression(Constant("2"))),
      repeatsMin = Some(TextExpression(Constant("1"))),
      fieldToTrack = Some(VariableInContext("repeatingSectionDriver")),
      fields = List(textFieldDos)
    )
    val formTemplate = basicFormTemplate.copy(sections = List(section1, section2))

    val textFieldDos1 = textFieldDos.copy(id = FieldId(s"1_${textFieldDos.id.value}"))
    val textFieldDos2 = textFieldDos.copy(id = FieldId(s"2_${textFieldDos.id.value}"))
    val sectionR1 = section2.copy(fields = List(textFieldDos1), title = "Repeating section title 1", shortName = Some("shortName 1"))
    val sectionR2 = section2.copy(fields = List(textFieldDos2), title = "Repeating section title 2", shortName = Some("shortName 2"))
    val expectedList = List(section1, sectionR1, sectionR2)

    val newFormData = mFormData.copy(fields = Seq(
      FormField(FieldId("1_DOS"), "@#~"),
      FormField(FieldId("2_DOS"), "--")
    ))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }
}
