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

package uk.gov.hmrc.gform.services

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.formtemplate.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.immutable.List

class RepeatingComponentServiceSpec extends Spec with ExampleData {

  val testService = RepeatingComponentService

  "getAllSections" should "return only sections in template when no repeating sections are defined" in {
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`))

    testService.getAllSections(form, formTemplate) shouldBe List(`section - group`)
  }

  it should "return no dynamically created sections when field in repeatsMax expression in repeating group and no form data" in {
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, `repeating section`))

    val expectedList = List(`section - group`)
    val formNoFormData = form.copy(formData = FormData(Seq.empty))

    testService.getAllSections(formNoFormData, formTemplate) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in repeating group, and non-empty form data" in {
    val thisGroup = `group - type`
      .copy(
        repeatsMax = Some(4),
        repeatsMin = Some(1),
        repeatLabel = Some(toLocalisedString("RepGrpLabel")),
        repeatAddAnotherText = Some(toLocalisedString("AddButtonLabel"))
      )

    val thisGroupFieldValue = `fieldValue - group`.copy(`type` = thisGroup)

    val thisSection1 = `section - group`.copy(fields = List(thisGroupFieldValue))

    val fieldDriver = `fieldId - firstName`.value
    val repeatingField = `fieldId - surname`.value
    val thisSection2 =
      `repeating section`
        .copy(
          title = toLocalisedString("""${n_""" + fieldDriver + """}, $n"""),
          shortName = Some(toLocalisedString("""$n, ${n_""" + fieldDriver + """}""")))

    val formTemplate = super.formTemplate.copy(sections = List(thisSection1, thisSection2))

    val textFieldR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val sectionR = thisSection2.copy(
      fields = List(textFieldR),
      title = toLocalisedString("ONE, 1"),
      shortName = Some(toLocalisedString("1, ONE")))

    val textFieldR2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
    val sectionR2 = thisSection2.copy(
      fields = List(textFieldR2),
      title = toLocalisedString("TWO, 2"),
      shortName = Some(toLocalisedString("2, TWO")))

    val expectedList = List(thisSection1, sectionR, sectionR2)

    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(fieldDriver), "ONE"),
        FormField(FormComponentId(s"1_$fieldDriver"), "TWO"),
        FormField(FormComponentId(s"1_$repeatingField"), "ONE"),
        FormField(FormComponentId(s"2_$repeatingField"), "TWO")
      ))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }

  it should "return a dynamically created section when field to track in a NON-repeating group" in {
    val thisSection2 = `repeating section`.copy(
      title = toLocalisedString("Repeating section title $n"),
      shortName = Some(toLocalisedString("shortName $n")))
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
    val textFieldDosR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val sectionR = thisSection2
      .copy(
        fields = List(textFieldDosR),
        title = toLocalisedString("Repeating section title 1"),
        shortName = Some(toLocalisedString("shortName 1")))
    val expectedList = List(`section - group`, sectionR)
    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(`fieldId - firstName`.value), "1"),
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "EEITT-866")))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in a NON-repeating group, with form data" in {
    val thisSection2 = `repeating section`.copy(
      title = toLocalisedString("Repeating section title $n"),
      shortName = Some(toLocalisedString("shortName $n")))
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
    val textFieldDos1 = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val textFieldDos2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
    val sectionR1 = thisSection2
      .copy(
        fields = List(textFieldDos1),
        title = toLocalisedString("Repeating section title 1"),
        shortName = Some(toLocalisedString("shortName 1")))
    val sectionR2 = thisSection2
      .copy(
        fields = List(textFieldDos2),
        title = toLocalisedString("Repeating section title 2"),
        shortName = Some(toLocalisedString("shortName 2")))
    val expectedList = List(`section - group`, sectionR1, sectionR2)

    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "@#~"),
        FormField(FormComponentId(s"2_${`fieldId - surname`.value}"), "!@£$%&*#")))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }
}
