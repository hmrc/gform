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
import uk.gov.hmrc.gform.formtemplate.SectionSyntax
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.RepeatingComponentService

import scala.collection.immutable.List

class RepeatingComponentServiceSpec extends Spec with ExampleData {

  val testService = RepeatingComponentService

  "getAllSections" should "return only sections in template when no repeating sections are defined" in {
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`))

    testService.getAllVisiblePages(form, formTemplate, None) shouldBe List(`section - group`.page)
  }

  it should "return no dynamically created sections when field in repeatsMax expression in repeating group and no form data" in {
    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, `repeating section`))

    val expectedList = List(`section - group`.page)
    val formNoFormData = form.copy(formData = FormData(Seq.empty))

    testService.getAllVisiblePages(formNoFormData, formTemplate, None) shouldBe expectedList
  }

  it should "return a dynamically created section when field to track in a NON-repeating group" in {
    val thisSection2 = `repeating section`
      .updateTitle(toSmartString("Repeating section title $n"))
      .updateShortName(toSmartString("shortName $n"))

    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))

    val textFieldDosR = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))

    val sectionR = thisSection2
      .updateTitle(toSmartString("Repeating section title 1"))
      .updateShortName(toSmartString("shortName 1"))
      .updateFields(List(textFieldDosR))

    val expectedList = List(`section - group`, sectionR).map(_.page)

    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(`fieldId - firstName`.value), "1"),
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "EEITT-866")))

    val newForm = form.copy(formData = newFormData)

    testService.getAllVisiblePages(newForm, formTemplate, None) shouldBe expectedList
  }

  it should "return dynamically created sections (title and shortName text built dynamically) when field to track in a NON-repeating group, with form data" in {
    val thisSection2 = `repeating section`
      .updateTitle(toSmartString("Repeating section title $n"))
      .updateShortName(toSmartString("shortName $n"))

    val formTemplate = super.formTemplate.copy(sections = List(`section - group`, thisSection2))

    val textFieldDos1 = `fieldValue - surname`.copy(id = FormComponentId(s"1_${`fieldId - surname`.value}"))

    val textFieldDos2 = `fieldValue - surname`.copy(id = FormComponentId(s"2_${`fieldId - surname`.value}"))

    val sectionR1 = thisSection2
      .updateTitle(toSmartString("Repeating section title 1"))
      .updateShortName(toSmartString("shortName 1"))
      .updateFields(List(textFieldDos1))

    val sectionR2 = thisSection2
      .updateTitle(toSmartString("Repeating section title 2"))
      .updateShortName(toSmartString("shortName 2"))
      .updateFields(List(textFieldDos2))

    val expectedList = List(`section - group`, sectionR1, sectionR2).map(_.page)

    val newFormData = FormData(
      fields = Seq(
        FormField(FormComponentId(s"1_${`fieldId - surname`.value}"), "@#~"),
        FormField(FormComponentId(s"2_${`fieldId - surname`.value}"), "!@£$%&*#")))

    val newForm = form.copy(formData = newFormData)

    testService.getAllVisiblePages(newForm, formTemplate, None) shouldBe expectedList
  }
}
