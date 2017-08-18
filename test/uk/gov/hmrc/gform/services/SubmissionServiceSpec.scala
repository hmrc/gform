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

import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyText, _ }
import uk.gov.hmrc.gform.submission.{ SectionFormField, SubmissionServiceHelper }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List

class SubmissionServiceSpec extends Spec {

  //TODO: benefit from ExampleData

  "SubmissionServiceHelper.getSectionFormFields" should "find repeating group fields" in {

    val formFields = Seq[FormField](
      FormField(FieldId("UNO"), "UNO"),
      FormField(FieldId("1_UNO"), "1_UNO"),
      FormField(FieldId("2_UNO"), "2_UNO"),
      FormField(FieldId("3_UNO"), "3_UNO"),
      FormField(FieldId("4_UNO"), "4_UNO"),
      FormField(FieldId("DOS"), "DOS"),
      FormField(FieldId("1_DOS"), "1_DOS"),
      FormField(FieldId("2_DOS"), "2_DOS"),
      FormField(FieldId("3_DOS"), "3_DOS"),
      FormField(FieldId("4_DOS"), "4_DOS")
    )
    val formData = FormData(formFields)

    val form = Form(FormId("MIO"), EnvelopeId(""), UserId("TESTID"), FormTemplateId("JustAFormTypeId"), None, formData)

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

    val textFieldDos = textFieldUno.copy(id = FieldId("DOS"), `type` = Text(AnyText, Constant("DOS"), false))

    val group = Group(
      fields = List(textFieldUno, textFieldDos),
      orientation = Horizontal,
      repeatsMax = Some(2),
      repeatsMin = Some(1),
      repeatLabel = Some("repeat label"),
      repeatAddAnotherText = Some("add group button label")
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
      None, None,
      fields = List(groupFieldValue)
    )

    val formTemplate = FormTemplate(
      _id = FormTemplateId("JustAFormTypeId"),
      formName = "formName",
      description = "formTemplateDescription",
      dmsSubmission = DmsSubmission("customerId", "classificationType", "businessArea"),
      AuthConfig(AuthConfigModule("TEST"), None, RegimeId("TEST")),
      submitSuccessUrl = "http://somwehere-nice.net",
      submitErrorUrl = "http://somwehere-nasty.net",
      sections = List(section),
      List.empty[AckSection]
    )

    val expectedResult = List(
      SectionFormField(
        "Section title",
        List(
          (
            List(FormField(FieldId("UNO"), "UNO")),
            FieldValue(FieldId("UNO"), Text(AnyText, Constant("UNO"), false), "Editable text label", None, None, true, true, true, None)
          ),
          (
            List(FormField(FieldId("DOS"), "DOS")),
            FieldValue(FieldId("DOS"), Text(AnyText, Constant("DOS"), false), "Editable text label", None, None, true, true, true, None)
          ),
          (
            List(FormField(FieldId("1_UNO"), "1_UNO")),
            FieldValue(FieldId("1_UNO"), Text(AnyText, Constant("UNO"), false), "Editable text label", None, None, true, true, true, None)
          ),
          (
            List(FormField(FieldId("1_DOS"), "1_DOS")),
            FieldValue(FieldId("1_DOS"), Text(AnyText, Constant("DOS"), false), "Editable text label", None, None, true, true, true, None)
          )
        )
      )
    )

    val res = SubmissionServiceHelper.getSectionFormFields(form, formTemplate)

    res.right.value should be(expectedResult)
  }

  implicit lazy val hc = new HeaderCarrier()
}
