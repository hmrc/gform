/*
 * Copyright 2020 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Helpers._
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

  it should "return a dynamically created section when field to track in a NON-repeating group" in {
    val thisSection2 = `repeating section`
      .copy(title = toSmartString("Repeating section title $n"), shortName = Some(toSmartString("shortName $n")))
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
    val textFieldDosR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val sectionR = thisSection2
      .copy(
        fields = List(textFieldDosR),
        title = toSmartString("Repeating section title 1"),
        shortName = Some(toSmartString("shortName 1")))
    val expectedList = List(`section - group`, sectionR)
    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(`fieldId - firstName`.value), "1"),
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "EEITT-866")))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in a NON-repeating group, with form data" in {
    val thisSection2 = `repeating section`
      .copy(title = toSmartString("Repeating section title $n"), shortName = Some(toSmartString("shortName $n")))
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))
    val textFieldDos1 = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))
    val textFieldDos2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))
    val sectionR1 = thisSection2
      .copy(
        fields = List(textFieldDos1),
        title = toSmartString("Repeating section title 1"),
        shortName = Some(toSmartString("shortName 1")))
    val sectionR2 = thisSection2
      .copy(
        fields = List(textFieldDos2),
        title = toSmartString("Repeating section title 2"),
        shortName = Some(toSmartString("shortName 2")))
    val expectedList = List(`section - group`, sectionR1, sectionR2)

    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "@#~"),
        FormField(FormComponentId(s"2_${`fieldId - surname`.value}"), "!@£$%&*#")))
    val newForm = form.copy(formData = newFormData)
    testService.getAllSections(newForm, formTemplate) shouldBe expectedList
  }
}
