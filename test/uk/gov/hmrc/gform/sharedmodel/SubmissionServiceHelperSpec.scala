/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ IncludeIf, IsFalse, IsTrue }
import uk.gov.hmrc.gform.submission.{ SectionFormField, SubmissionServiceHelper }

class SubmissionServiceHelperSpec extends Spec {

  "getSectionFormFields" should "return a Right if formData is present for all fields" in new ExampleData {
    SubmissionServiceHelper.getSectionFormFields(form, formTemplate, None).right.value
  }

  it should "return a Left if formData is missing" in new ExampleData {
    override lazy val formData = super.formData.copy(formFields.init)
    val sectionFormFieldsOpt: Opt[List[SectionFormField]] =
      SubmissionServiceHelper.getSectionFormFields(form, formTemplate, None)
    sectionFormFieldsOpt.left.value shouldBe UnexpectedState("No formField for field.id: startDate found")
  }

  it should "filter not included Sections" in new ExampleData {
    override val `section - about you` = super.`section - about you`.copy(includeIf = Some(IncludeIf(IsFalse)))
    val sections = SubmissionServiceHelper.getSectionFormFields(form, formTemplate, None).right.value
    val expectedSize = formTemplate.sections.size // this includes the declaration  section
    sections.size shouldBe expectedSize withClue "sections included"
    sections.map(_.title) should not contain `fieldValue - firstName`.label
    sections.map(_.title) should not contain `fieldValue - surname`.label
    sections.map(_.title) should not contain `fieldValue - facePhoto`.label
  }

  it should "return only the formData on included Sections" in new ExampleData {
    override val `section - about you` = super.`section - about you`.copy(includeIf = Some(IncludeIf(IsTrue)))
    val sectionFormFields1 = SubmissionServiceHelper.getSectionFormFields(form, formTemplate, None).right.get
    sectionFormFields1.size shouldBe formTemplate.sections.size + 1 // This includes the declaration section

    sectionFormFields1.map(_.title) should contain allOf (`section - about you`.title,
    `section - businessDetails`.title)
  }
}
