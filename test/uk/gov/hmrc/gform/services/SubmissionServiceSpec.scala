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

import java.time.LocalDateTime

import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.submission.{SectionFormField, SubmissionServiceHelper}

import scala.collection.immutable.List
import uk.gov.hmrc.http.HeaderCarrier

class SubmissionServiceSpec extends Spec {

  //TODO: benefit from ExampleData

  "SubmissionServiceHelper.getSectionFormFields" should "find repeating group fields" in {

    val formFields = Seq[FormField](
      FormField(FormComponentId("UNO"), "UNO"),
      FormField(FormComponentId("1_UNO"), "1_UNO"),
      FormField(FormComponentId("2_UNO"), "2_UNO"),
      FormField(FormComponentId("3_UNO"), "3_UNO"),
      FormField(FormComponentId("4_UNO"), "4_UNO"),
      FormField(FormComponentId("DOS"), "DOS"),
      FormField(FormComponentId("1_DOS"), "1_DOS"),
      FormField(FormComponentId("2_DOS"), "2_DOS"),
      FormField(FormComponentId("3_DOS"), "3_DOS"),
      FormField(FormComponentId("4_DOS"), "4_DOS")
    )
    val formData = FormData(formFields)

    val form = Form(
      FormId("MIO"),
      EnvelopeId(""),
      uk.gov.hmrc.gform.sharedmodel.UserId("TESTID"),
      FormTemplateId("JustAFormTypeId"),
      None,
      formData,
      InProgress,
      Some(EnvelopeExpiryDate(new LocalDateTime)))

    val textFieldUno = FormComponent(
      id = FormComponentId("UNO"),
      `type` = Text(AnyText, Constant("UNO")),
      label = "Editable text label",
      helpText = None,
      shortName = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = false,
      errorMessage = None
    )

    val textFieldDos =
      textFieldUno.copy(id = FormComponentId("DOS"), `type` = Text(AnyText, Constant("DOS")))

    val group = Group(
      fields = List(textFieldUno, textFieldDos),
      orientation = Horizontal,
      repeatsMax = Some(2),
      repeatsMin = Some(1),
      repeatLabel = Some("repeat label"),
      repeatAddAnotherText = Some("add group button label")
    )

    val groupFieldValue = FormComponent(
      id = FormComponentId("GroupFieldValueId"),
      `type` = group,
      label = "group FieldValue label",
      helpText = None,
      shortName = None,
      validIf = None,
      mandatory = true,
      editable = false,
      submissible = true,
      derived = false,
      onlyShowOnSummary = false,
      errorMessage = None
    )

    val section = Section(
      title = "Section title",
      description = None,
      progressIndicator = None,
      shortName = None,
      includeIf = None,
      None,
      None,
      None,
      fields = List(groupFieldValue),
      None,
      None)

    val formTemplate = FormTemplate.withDeprecatedDmsSubmission(
      _id = FormTemplateId("JustAFormTypeId"),
      formName = "formName",
      description = "formTemplateDescription",
      developmentPhase = Some(ResearchBanner),
      formCategory = Some(Default),
      draftRetrievalMethod = None,
      submissionReference = None,
      dmsSubmission =
        DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "classificationType", "businessArea"),
      authConfig = HmrcAgentWithEnrolmentModule(
        RequireMTDAgentEnrolment,
        EnrolmentAuth(ServiceId("TEST"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("TEST"))))),
      emailTemplateId = "test-email-template-id",
      submitSuccessUrl = "http://somwehere-nice.net",
      submitErrorUrl = "http://somwehere-nasty.net",
      sections = List(section),
      acknowledgementSection = AcknowledgementSection("", None, None, Nil),
      declarationSection = DeclarationSection("Declaration", None, None, Nil)
    )

    val expectedResult = List(
      SectionFormField(
        "Section title",
        List(
          (
            List(FormField(FormComponentId("UNO"), "UNO")),
            FormComponent(
              FormComponentId("UNO"),
              Text(AnyText, Constant("UNO")),
              "Editable text label",
              None,
              None,
              None,
              true,
              true,
              true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("DOS"), "DOS")),
            FormComponent(
              FormComponentId("DOS"),
              Text(AnyText, Constant("DOS")),
              "Editable text label",
              None,
              None,
              None,
              true,
              true,
              true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("1_UNO"), "1_UNO")),
            FormComponent(
              FormComponentId("1_UNO"),
              Text(AnyText, Constant("UNO")),
              "Editable text label",
              None,
              None,
              None,
              true,
              true,
              true,
              derived = false,
              onlyShowOnSummary = false,
              None
            )),
          (
            List(FormField(FormComponentId("1_DOS"), "1_DOS")),
            FormComponent(
              FormComponentId("1_DOS"),
              Text(AnyText, Constant("DOS")),
              "Editable text label",
              None,
              None,
              None,
              true,
              true,
              true,
              derived = false,
              onlyShowOnSummary = false,
              None
            ))
        )
      ),
      SectionFormField("Declaration", List())
    )

    val res = SubmissionServiceHelper.getSectionFormFields(form, formTemplate, None)

    res.right.value should be(expectedResult)
  }

  implicit lazy val hc = new HeaderCarrier()
}
