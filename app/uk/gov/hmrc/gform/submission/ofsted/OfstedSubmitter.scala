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

import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsDestinationResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.submission.DestinationsSubmitterAlgebra
import uk.gov.hmrc.http.HeaderCarrier

trait OfstedSubmitter[F[_]] {
  def submitForReview(
    submissionInfo: DestinationSubmissionInfo,
    userId: UserId,
    formTemplateId: FormTemplateId,
    correlationFormFieldId: FormComponentId)(implicit hc: HeaderCarrier): F[FormId]

  def reject(reviewFormId: FormId, correlationFormFieldId: FormComponentId, commentFormFieldId: FormComponentId)(
    implicit hc: HeaderCarrier): F[Unit]

  def approve(
    reviewFormId: FormId,
    correlationFormFieldId: FormComponentId,
    submitter: DestinationsSubmitterAlgebra[F],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Unit]
}

class RealOfstedSubmitter[F[_]](
  formAlgebra: FormAlgebra[F],
  formTemplateAlgebra: FormTemplateAlgebra[F],
  reviewNotificationService: OfstedReviewNotificationAlgebra[F])(implicit F: MonadError[F, String])
    extends OfstedSubmitter[F] {

  def approve(
    reviewFormId: FormId,
    correlationFormFieldId: FormComponentId,
    submitter: DestinationsSubmitterAlgebra[F],
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      reviewForm     <- formAlgebra.get(reviewFormId)
      reviewedFormId <- findReviewedFormId(reviewForm, correlationFormFieldId)
      _              <- replayDestinations(submitter, reviewedFormId)
      _              <- formAlgebra.updateFormStatus(reviewFormId, Submitted)
      _              <- formAlgebra.updateFormStatus(reviewedFormId, Submitted)
      _              <- reviewNotificationService.approve(reviewedFormId)
    } yield ()

  private def replayDestinations(submitter: DestinationsSubmitterAlgebra[F], reviewedFormId: FormId)(
    implicit hc: HeaderCarrier): F[Unit] =
    for {
      form         <- formAlgebra.get(reviewedFormId)
      formTemplate <- formTemplateAlgebra.get(form.formTemplateId)
      _            <- processDestinations(submitter, reviewedFormId, form, formTemplate)
    } yield ()

  private def processDestinations(
    submitter: DestinationsSubmitterAlgebra[F],
    reviewedFormId: FormId,
    form: Form,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): F[Option[HandlebarsDestinationResponse]] =
    form.destinationSubmissionInfo
      .map(processDestinationsWithSubmissionInfo(submitter, reviewedFormId, form, formTemplate, _))
      .getOrElse(F.raiseError(RealOfstedSubmitter.missingDestinationSubmissionInfo(reviewedFormId)))

  private def processDestinationsWithSubmissionInfo(
    submitter: DestinationsSubmitterAlgebra[F],
    reviewedFormId: FormId,
    form: Form,
    formTemplate: FormTemplate,
    info: DestinationSubmissionInfo)(implicit hc: HeaderCarrier): F[Option[HandlebarsDestinationResponse]] =
    for {
      _      <- formAlgebra.updateFormStatus(reviewedFormId, Approved)
      result <- submitter.send(info, formTemplate, formAlgebra)
    } yield result

  def reject(reviewFormId: FormId, correlationFormFieldId: FormComponentId, commentFormFieldId: FormComponentId)(
    implicit hc: HeaderCarrier): F[Unit] =
    for {
      reviewForm       <- formAlgebra.get(reviewFormId)
      reviewedFormId   <- findReviewedFormId(reviewForm, correlationFormFieldId)
      rejectionMessage <- findRejectionMessage(reviewForm, commentFormFieldId)
      _                <- formAlgebra.updateFormStatus(reviewFormId, Submitted)
      _                <- formAlgebra.updateFormStatus(reviewedFormId, InProgress)
      _                <- reviewNotificationService.reject(reviewedFormId, rejectionMessage)
    } yield ()

  def submitForReview(
    submissionInfo: DestinationSubmissionInfo,
    userId: UserId,
    formTemplateId: FormTemplateId,
    correlationFieldId: FormComponentId)(implicit hc: HeaderCarrier): F[FormId] =
    for {
      _      <- formAlgebra.updateFormStatus(submissionInfo.formId, NeedsReview)
      formId <- createReviewForm(submissionInfo.formId, userId, formTemplateId, correlationFieldId)
      _      <- reviewNotificationService.requestReview(formId)
    } yield formId

  private def findReviewedFormId(reviewForm: Form, correlationFormFieldId: FormComponentId) =
    findFormComponentValue(
      reviewForm,
      correlationFormFieldId,
      RealOfstedSubmitter.missingReviewedFormId(reviewForm._id, correlationFormFieldId)
    ).map(v => FormId(v))

  private def findRejectionMessage(reviewForm: Form, commentFormFieldId: FormComponentId) =
    findFormComponentValue(
      reviewForm,
      commentFormFieldId,
      RealOfstedSubmitter.missingRejectionMessage(reviewForm._id, commentFormFieldId))

  private def findFormComponentValue(form: Form, componentId: FormComponentId, error: String): F[String] =
    form.formData.fields
      .find(_.id === componentId)
      .map(_.value.pure)
      .getOrElse(F.raiseError(error))

  private def createReviewForm(
    idOfFormToBeReviewed: FormId,
    userId: UserId,
    formTemplateId: FormTemplateId,
    correlationFieldId: FormComponentId)(implicit hc: HeaderCarrier) =
    formAlgebra.create(
      userId,
      formTemplateId,
      Some(AccessCode.random),
      Int.MaxValue.longValue,
      Seq(FormField(correlationFieldId, idOfFormToBeReviewed.value))
    )
}

object RealOfstedSubmitter {
  def missingDestinationSubmissionInfo(reviewedFormId: FormId) =
    s"Unable to find destination info for processing reviewed form ${reviewedFormId.value}"

  def missingReviewedFormId(reviewFormId: FormId, correlationFormFieldId: FormComponentId) =
    s"The review form with id '${reviewFormId.value}' has no value for the correlationFormFieldId '${correlationFormFieldId.value}'"

  def missingRejectionMessage(reviewFormId: FormId, commentFormFieldId: FormComponentId) =
    s"The review form with id '${reviewFormId.value}' has no value for the commentFormFieldId '${commentFormFieldId.value}'"
}
