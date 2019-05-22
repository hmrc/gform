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

package uk.gov.hmrc.gform.submission.ofsted

import cats.Applicative
import cats.data.NonEmptyList
import cats.syntax.applicative._
import play.api.libs.json.JsNull
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ UserId => SMUserId, _ }
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationId, HandlebarsDestinationResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.ReviewApproval
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.DestinationsSubmitterAlgebra
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.http.HeaderCarrier

class RealOfstedSubmitterSpec extends Spec {
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "submitForReview" should "create an Ofsted reviewer form with a correlation ID and send an email" in {
    val submitterParts = createSubmitter

    val idOfFormToBeReviewed = FormId("MIO")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")
    val reviewFormTemplateId = FormTemplateId("reviewFormTemplateId")
    val userId = SMUserId("userId")

    val correlationFormField = Seq(FormField(correlationIdFormComponentId, idOfFormToBeReviewed.value))
    val newFormId = FormId("newForm")
    val submissionInfo = DestinationSubmissionInfo(
      idOfFormToBeReviewed,
      "customer",
      None,
      SubmissionData(
        "pdfHtml",
        Variables(JsNull),
        StructuredFormValue.ObjectStructure(Nil),
        EmailParametersRecalculated(Map.empty))
    )

    submitterParts
      .expectUpdateFormStatus(idOfFormToBeReviewed, NeedsReview, NeedsReview)
      .expectCreateForm(newFormId, userId, reviewFormTemplateId, correlationFormField)
      .expectRequestReviewNotification(newFormId)
      .sut
      .submitForReview(submissionInfo, userId, reviewFormTemplateId, correlationIdFormComponentId)
      .shouldBe(newFormId.pure)
  }

  private val reviewFormTemplateId = FormTemplateId("reviewFormTemplateId")
  "reject" should "change the workflow state of the review form to Submitted, set the reviewed form's workflow state to InProgress and send an email to the reviewed form's user" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val reviewedFormId = FormId("reviewedForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")
    val rejectionComment = "You shall not pass!"
    val rejectionCommentFormComponentId = FormComponentId("rejectionComment")

    submitterParts
      .expectUpdateFormStatus(reviewFormId, Submitted, Submitted)
      .expectRetrieveForm(
        reviewFormId,
        createForm(
          reviewFormId,
          reviewFormTemplateId,
          Submitted,
          (correlationIdFormComponentId, reviewedFormId.value),
          (rejectionCommentFormComponentId, rejectionComment))
      )
      .expectUpdateFormStatus(reviewedFormId, InProgress, InProgress)
      .expectReviewRejectionNotification(reviewedFormId, rejectionComment)
      .sut
      .reject(reviewFormId, correlationIdFormComponentId, rejectionCommentFormComponentId)
      .shouldBe(().pure)
  }

  it should "fail without changing any workflow states if the correlationIdFormComponent cannot be found in the review form" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")
    val rejectionComment = "You shall not pass!"
    val rejectionCommentFormComponentId = FormComponentId("rejectionComment")

    submitterParts
      .expectRetrieveForm(
        reviewFormId,
        createForm(reviewFormId, reviewFormTemplateId, Submitted, (rejectionCommentFormComponentId, rejectionComment))
      )
      .sut
      .reject(reviewFormId, correlationIdFormComponentId, rejectionCommentFormComponentId)
      .shouldBe(Left(RealOfstedSubmitter.missingReviewedFormId(reviewFormId, correlationIdFormComponentId)))
  }

  it should "fail without changing any workflow states if the rejectionCommentFormComponentId cannot be found in the review form" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val reviewedFormId = FormId("reviewedForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")
    val rejectionCommentFormComponentId = FormComponentId("rejectionComment")

    submitterParts
      .expectRetrieveForm(
        reviewFormId,
        createForm(reviewFormId, reviewFormTemplateId, Submitted, (correlationIdFormComponentId, reviewedFormId.value))
      )
      .sut
      .reject(reviewFormId, correlationIdFormComponentId, rejectionCommentFormComponentId)
      .shouldBe(Left(RealOfstedSubmitter.missingRejectionMessage(reviewFormId, rejectionCommentFormComponentId)))
  }

  "approve" should "set the reviewed form's workflow state to Approved, re-run the reviewed form's destinations, change both forms' statuses to Submitted, and send an email to the reviewed form's user" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val reviewedFormId = FormId("reviewedForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")
    val reviewedFormTemplateId = FormTemplateId("reviewedFormTemplateId")
    val reviewedFormTemplate = createFormTemplate(reviewedFormTemplateId)

    val destinationSubmissionInfo = DestinationSubmissionInfo(
      reviewedFormId,
      "id",
      None,
      SubmissionData(
        "pdf",
        Variables(null),
        StructuredFormValue.ObjectStructure(Nil),
        EmailParametersRecalculated(Map.empty)))

    submitterParts
      .expectRetrieveForm(
        reviewFormId,
        createForm(reviewFormId, reviewFormTemplateId, Submitted, (correlationIdFormComponentId, reviewedFormId.value))
      )
      .expectRetrieveForm(
        reviewedFormId,
        createForm(
          reviewedFormId,
          reviewedFormTemplateId,
          Submitted,
          Some(destinationSubmissionInfo),
          (correlationIdFormComponentId, reviewedFormId.value))
      )
      .expectRetrieveFormTemplate(reviewedFormTemplateId, reviewedFormTemplate)
      .expectUpdateFormStatus(reviewedFormId, Approved, Approved)
      .expectDestinationsSubmitterSend(destinationSubmissionInfo, reviewedFormTemplate)
      .expectUpdateFormStatus(reviewFormId, Submitted, Submitted)
      .expectUpdateFormStatus(reviewedFormId, Submitted, Submitted)
      .expectReviewApprovalNotification(reviewedFormId)
      .sut
      .approve(
        reviewFormId,
        correlationIdFormComponentId,
        submitterParts.destinationsSubmitterAlgebra,
        createFormTemplate(reviewFormTemplateId))
      .shouldBe(().pure)
  }

  it should "fail without changing the reviewed states if the reviewed form does not contains destination submission info" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val reviewedFormId = FormId("reviewedForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")

    val reviewedFormTemplateId = FormTemplateId("reviewedFormTemplateId")

    submitterParts
      .expectRetrieveForm(
        reviewFormId,
        createForm(reviewFormId, reviewFormTemplateId, InProgress, (correlationIdFormComponentId, reviewedFormId.value))
      )
      .expectRetrieveForm(
        reviewedFormId,
        createForm(
          reviewedFormId,
          reviewedFormTemplateId,
          NeedsReview,
          None,
          (correlationIdFormComponentId, reviewedFormId.value))
      )
      .expectRetrieveFormTemplate(reviewedFormTemplateId, createFormTemplate(reviewedFormTemplateId))
      .sut
      .approve(
        reviewFormId,
        correlationIdFormComponentId,
        submitterParts.destinationsSubmitterAlgebra,
        createFormTemplate(reviewFormTemplateId))
      .shouldBe(Left(RealOfstedSubmitter.missingDestinationSubmissionInfo(reviewedFormId)))
  }

  it should "fail without changing any workflow states if the correlationIdFormComponent cannot be found in the review form" in {
    val submitterParts = createSubmitter

    val reviewFormId = FormId("reviewForm")
    val correlationIdFormComponentId = FormComponentId("correlationIdFormComponentId")

    submitterParts
      .expectRetrieveForm(
        reviewFormId,
        createForm(reviewFormId, reviewFormTemplateId, Submitted)
      )
      .sut
      .approve(
        reviewFormId,
        correlationIdFormComponentId,
        submitterParts.destinationsSubmitterAlgebra,
        createFormTemplate(reviewFormTemplateId))
      .shouldBe(Left(RealOfstedSubmitter.missingReviewedFormId(reviewFormId, correlationIdFormComponentId)))
  }

  private def createForm(
    id: FormId,
    formTemplateId: FormTemplateId,
    status: FormStatus,
    formData: (FormComponentId, String)*): Form =
    createForm(id, formTemplateId, status, None, formData: _*)

  private def createForm(
    id: FormId,
    formTemplateId: FormTemplateId,
    status: FormStatus,
    destinationSubmissionInfo: Option[DestinationSubmissionInfo],
    formData: (FormComponentId, String)*): Form =
    Form(
      id,
      null,
      null,
      formTemplateId,
      FormData(formData.map { case (id, v) => FormField(id, v) }),
      status,
      VisitIndex.empty,
      ThirdPartyData.empty,
      None,
      destinationSubmissionInfo
    )

  private def createFormTemplate(id: FormTemplateId) =
    FormTemplate(
      id,
      toLocalisedString(""),
      toLocalisedString(""),
      None,
      Default,
      None,
      None,
      DestinationList(NonEmptyList.one(ReviewApproval(DestinationId("id"), FormComponentId("bar")))),
      None,
      Anonymous,
      "",
      None,
      "",
      "",
      None,
      Nil,
      AcknowledgementSection(toLocalisedString(""), None, None, Nil),
      DeclarationSection(toLocalisedString(""), None, None, Nil),
      None,
      AvailableLanguages.default
    )

  case class SubmitterParts[M[_]: Applicative](
    sut: RealOfstedSubmitter[M],
    destinationsSubmitterAlgebra: DestinationsSubmitterAlgebra[M],
    notificationAlgebra: OfstedReviewNotificationAlgebra[M],
    formAlgebra: FormAlgebra[M],
    formTemplateAlgebra: FormTemplateAlgebra[M]) {
    def expectRetrieveForm(formId: FormId, form: Form): SubmitterParts[M] = {
      (formAlgebra
        .get(_: FormId)(_: HeaderCarrier))
        .stubs(formId, hc)
        .returning(form.pure[M])
      this
    }

    def expectReviewRejectionNotification(formId: FormId, rejectionComment: String): SubmitterParts[M] = {
      (notificationAlgebra
        .reject(_: FormId, _: String))
        .expects(formId, rejectionComment)
        .returning(().pure[M])
      this
    }

    def expectRetrieveFormTemplate(formTemplateId: FormTemplateId, formTemplate: FormTemplate): SubmitterParts[M] = {
      (formTemplateAlgebra
        .get(_: FormTemplateId))
        .expects(formTemplateId)
        .returning(formTemplate.pure[M])
      this
    }

    def expectDestinationsSubmitterSend(
      destinationSubmissionInfo: DestinationSubmissionInfo,
      formTemplate: FormTemplate): SubmitterParts[M] = {
      (destinationsSubmitterAlgebra
        .send(_: DestinationSubmissionInfo, _: FormTemplate, _: FormAlgebra[M])(_: HeaderCarrier))
        .expects(destinationSubmissionInfo, formTemplate, formAlgebra, hc)
        .returning(Option.empty[HandlebarsDestinationResponse].pure[M])
      this
    }

    def expectReviewApprovalNotification(formId: FormId): SubmitterParts[M] = {
      (notificationAlgebra
        .approve(_: FormId))
        .expects(formId)
        .returning(().pure[M])
      this
    }

    def expectRequestReviewNotification(formId: FormId): SubmitterParts[M] = {
      (notificationAlgebra
        .requestReview(_: FormId))
        .expects(formId)
        .returning(().pure[M])
      this
    }

    def expectUpdateFormStatus(formId: FormId, status: FormStatus, newStatus: FormStatus): SubmitterParts[M] = {
      (formAlgebra
        .updateFormStatus(_: FormId, _: FormStatus)(_: HeaderCarrier))
        .expects(formId, status, hc)
        .returning(newStatus.pure[M])
      this
    }

    def expectCreateForm(
      newFormId: FormId,
      userId: SMUserId,
      reviewFormTemplateId: FormTemplateId,
      initialFields: Seq[FormField]): SubmitterParts[M] = {
      (formAlgebra
        .create(_: SMUserId, _: FormTemplateId, _: Option[AccessCode], _: Long, _: Seq[FormField])(_: HeaderCarrier))
        .expects(where {
          (
            expectedUserId: SMUserId,
            expectedReviewFormTemplateId: FormTemplateId,
            accessCode: Option[AccessCode],
            expiryTime: Long,
            expectedInitialFields: Seq[FormField],
            hc: HeaderCarrier) =>
            userId == expectedUserId &&
            reviewFormTemplateId == expectedReviewFormTemplateId &&
            accessCode.isDefined &&
            initialFields == expectedInitialFields
        })
        .returning(newFormId.pure[M])
      this
    }
  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val formAlgebra = mock[FormAlgebra[Possible]]
    val notificationAlgebra = mock[OfstedReviewNotificationAlgebra[Possible]]
    val formTemplateAlgebra = mock[FormTemplateAlgebra[Possible]]
    val destinationsSubmitterAlgebra = mock[DestinationsSubmitterAlgebra[Possible]]
    val submitter = new RealOfstedSubmitter[Possible](formAlgebra, formTemplateAlgebra, notificationAlgebra)
    SubmitterParts(submitter, destinationsSubmitterAlgebra, notificationAlgebra, formAlgebra, formTemplateAlgebra)
  }
}
