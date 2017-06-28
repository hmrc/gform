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

package uk.gov.hmrc.gform.models

import org.scalatest._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.services.SubmissionService

class SubmissionServiceSpec extends FlatSpec with Matchers with EitherValues {

  val section0 = Section("Section for Start Date", None, None, List(
    FieldValue(FieldId("startDate"), Date(AnyDate, Offset(0), None), "Your Start Date", None, None, true, true, true)
  ))

  val section1 = Section("About you", None, None, List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, None, true, true, true)))

  val section2 = Section("Business details", None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, None, true, true, true)))

  val formTemplate = FormTemplate(
    schemaId = None,
    formTypeId = FormTypeId(""),
    formName = "IPT100",
    version = Version("1.2.3"),
    description = "abc",
    characterSet = "UTF-8",
    dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area"),
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )

  val data = Map(
    FieldId("startDate.year") -> FormField(FieldId("startDate.year"), "2010"),
    FieldId("startDate.day") -> FormField(FieldId("startDate.day"), "10"),
    FieldId("startDate.month") -> FormField(FieldId("startDate.month"), "10"),
    FieldId("firstName") -> FormField(FieldId("firstName"), "Pete"),
    FieldId("nameOfBusiness") -> FormField(FieldId("nameOfBusiness"), "Business Name")
  )

  val formData = Form(FormId("anId"), FormData(FormTypeId("ftid"), Version("version"), "charset", data.values.toSeq))

  "getSectionFormFields" should "return a Right if formData is present for all fields" in {

    SubmissionService.getSectionFormFields(formData, formTemplate) should be('right)

  }

  it should "return a Left if formData is missing" in {

    val formDataWithoutRequiredField = Form(FormId("anId"), FormData(FormTypeId("ftid"), Version("version"), "charset", (data - FieldId("startDate.month")).values.toSeq))

    val sectionFormFieldsOpt = SubmissionService.getSectionFormFields(formDataWithoutRequiredField, formTemplate)

    sectionFormFieldsOpt should be('left)
    sectionFormFieldsOpt.left.get shouldBe (InvalidState("No formField for field.id: startDate found"))

  }

  it should "return a Right if formData is present for the fields on included Sections" in {

    val excludedSection = section0.copy(includeIf = Some(IncludeIf(IsFalse)))
    val formDataWithoutFieldsOnExludedSection = Form(FormId("anId"), FormData(FormTypeId("ftid"), Version("version"), "charset", (data - FieldId("startDate.month")).values.toSeq))
    val ftWithExcludedSection0 = formTemplate.copy(sections = List(excludedSection, section1, section2))

    SubmissionService.getSectionFormFields(formDataWithoutFieldsOnExludedSection, ftWithExcludedSection0) should be('right)
  }
}
