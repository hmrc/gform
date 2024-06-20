/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.form

import cats.Monad
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.formmetadata.{ FormMetadata, FormMetadataAlgebra }
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.save4later.FormPersistenceAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ FormStatus, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, FormAccessCodeForAgents, FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import java.time.temporal.ChronoUnit
import java.time.Instant

class FormService[F[_]: Monad](
  formPersistence: FormPersistenceAlgebra[F],
  fileUpload: FileUploadAlgebra[F],
  formTemplateAlgebra: FormTemplateAlgebra[F],
  formMetadataAlgebra: FormMetadataAlgebra[F],
  formExpiryDays: Int,
  formExpiryDaysFromCreation: Int
) extends FormAlgebra[F] {

  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form] =
    formPersistence.get(formId)

  def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): F[Form] =
    formPersistence.get(formIdData)

  private val allowedStates: Set[FormStatus] = Set(InProgress, Summary, Validated, Signed, NeedsReview)

  def getAll(userId: UserId, formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): F[List[FormOverview]] =
    for {
      formMetadatas <- formMetadataAlgebra.getAll(userId, formTemplateId)
      existingForms <- formMetadatas.traverse { formMetadata =>
                         formPersistence
                           .find(formMetadata._id)
                           .map(_.map(form => (form, formMetadata)))
                       }
    } yield {
      val filteredForms: List[(Form, FormMetadata)] = existingForms.flatten.filter { case (form, _) =>
        allowedStates.contains(form.status)
      }

      filteredForms.map { case (_, formMetadata) => FormOverview.fromFormMetadata(formMetadata) }
    }

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit] =
    formPersistence.delete(formId)

  def unstuck(formId: FormId)(implicit hc: HeaderCarrier): F[Unit] =
    forceUpdateFormStatus(formId, InProgress)

  private def createNewFormData(
    userId: UserId,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId,
    affinityGroup: Option[AffinityGroup]
  ): FormIdData = {
    val formTemplateId = formTemplate._id
    (formTemplate.draftRetrievalMethod, affinityGroup) match {

      case (BySubmissionReference, _) =>
        val submissionRef = SubmissionRef(envelopeId)
        FormIdData.WithAccessCode(userId, formTemplateId, AccessCode(submissionRef.value))

      case (FormAccessCodeForAgents(_), Some(AffinityGroup.Agent)) =>
        val ac = AccessCode.random
        FormIdData.WithAccessCode(userId, formTemplateId, ac)

      case _ => FormIdData.Plain(userId, formTemplateId)
    }
  }

  def create(
    userId: UserId,
    formTemplateId: FormTemplateId,
    affinityGroup: Option[AffinityGroup],
    queryParams: QueryParams
  )(implicit hc: HeaderCarrier): F[FormIdData] = {
    val timeProvider = new TimeProvider
    val expiryDate = timeProvider.localDateTime().plusDays(formExpiryDays.toLong)

    for {
      formTemplate <- formTemplateAlgebra.get(formTemplateId)
      envelopeId <-
        fileUpload.createEnvelope(
          formTemplateId,
          formTemplate.allowedFileTypes,
          expiryDate,
          formTemplate.fileSizeLimit,
          formTemplate.isObjectStore
        )
      formIdData = createNewFormData(userId, formTemplate, envelopeId, affinityGroup)
      lowerCased = formIdData.lowerCaseId
      form = Form(
               lowerCased.toFormId,
               envelopeId,
               userId,
               lowerCased.formTemplateId,
               Some(formTemplate.version),
               FormData(fields = Seq.empty),
               InProgress,
               VisitIndex.empty(formTemplate.formKind),
               ThirdPartyData.empty.copy(queryParams = queryParams),
               Some(EnvelopeExpiryDate(expiryDate)),
               FormComponentIdToFileIdMapping.empty
             )
      _ <- formPersistence.upsert(form)
      _ <- formMetadataAlgebra.upsert(lowerCased)
    } yield lowerCased
  }

  def updateUserData(formIdData: FormIdData, userData: UserData)(implicit hc: HeaderCarrier): F[Unit] = {

    val lowerCased = formIdData.lowerCaseId
    for {
      form         <- get(formIdData)
      formMetaData <- formMetadataAlgebra.get(formIdData)
      formData = if (lowerCased.formTemplateId.isSpecimen) FormData(Seq.empty[FormField]) else userData.formData
      expiryDate = calcExpiryDate(formMetaData.createdAt)
      newForm = form
                  .copy(
                    _id = lowerCased.toFormId,
                    formTemplateId = lowerCased.formTemplateId,
                    formData = formData,
                    status = newStatus(form, userData.formStatus),
                    visitsIndex = userData.visitsIndex,
                    thirdPartyData = userData.thirdPartyData,
                    componentIdToFileId = userData.componentIdToFileId,
                    envelopeExpiryDate = Some(EnvelopeExpiryDate(expiryDate))
                  )
      _ <- formPersistence.upsert(newForm)
      _ <- refreshMetadata(form.formData != newForm.formData, lowerCased, newForm.formData)
    } yield ()
  }

  private def calcExpiryDate(createdAt: Instant) = {
    val timeProvider = new TimeProvider
    val daysSinceCreation = ChronoUnit.DAYS.between(createdAt, timeProvider.instant())
    if (daysSinceCreation + formExpiryDays >= formExpiryDaysFromCreation) {
      val remainingDays = Math.max(formExpiryDaysFromCreation - daysSinceCreation, 0)
      timeProvider.localDateTime().plusDays(remainingDays)
    } else {
      timeProvider.localDateTime().plusDays(formExpiryDays.toLong)
    }
  }
  def createFormFromLegacy(formIdData: FormIdData, newFormIdData: FormIdData)(implicit hc: HeaderCarrier): F[Form] = {
    val lowerCased = newFormIdData.lowerCaseId

    for {
      form <- get(formIdData)
      newForm = form.copy(_id = newFormIdData.toFormId, formTemplateId = newFormIdData.formTemplateId)
      _ <- formPersistence.upsert(newForm)
      _ <- formMetadataAlgebra.upsert(lowerCased)
      _ <- formPersistence.delete(formIdData.toFormId)
      _ <- formMetadataAlgebra.delete(formIdData)
    } yield newForm
  }

  private def refreshMetadata(toRefresh: Boolean, formIdData: FormIdData, formData: FormData) =
    if (toRefresh) for {
      formTemplate <- formTemplateAlgebra.get(formIdData.formTemplateId)
      parentRefs = resolveParentFormSubmissionRefs(formTemplate.parentFormSubmissionRefs, formData)
      _ <- formMetadataAlgebra.touch(formIdData, parentRefs)
    } yield ()
    else implicitly[Monad[F]].pure(())

  private def resolveParentFormSubmissionRefs(fcIds: List[FormComponentId], formData: FormData): List[SubmissionRef] =
    fcIds.flatMap(fcId => formData.find(fcId).map(SubmissionRef(_)))

  def changeVersion(formIdData: FormIdData, formTemplateId: FormTemplateId)(implicit
    hc: HeaderCarrier
  ): F[Form] =
    for {
      form <- get(formIdData)
      newForm = form.copy(
                  _id = formIdData.toFormIdWithFormTemplateId(formTemplateId),
                  formTemplateId = formTemplateId
                )
      _ <- formPersistence.upsert(newForm)
      _ <- refreshMetadata(true, formIdData, form.formData)
    } yield newForm

  def updateFormStatus(formId: FormId, status: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus] =
    for {
      form <- get(formId)
      newS = newStatus(form, status)
      _ <- formPersistence.upsert(form.copy(status = newS))
    } yield newS

  def forceUpdateFormStatus(formIdData: FormIdData, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    forceUpdateFormStatus(formIdData.toFormId, newStatus)

  def forceUpdateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      form <- get(formId)
      _    <- formPersistence.upsert(form.copy(status = newStatus))
    } yield ()

  private def newStatus(form: Form, status: FormStatus) =
    LifeCycleStatus.newStatus(form, status)

}

object LifeCycleStatus {
  def newStatus(form: Form, status: FormStatus): FormStatus =
    apply(form.status, status) match {
      case Some(v) =>
        Loggers.stateTransitions.info(formatLogMessage(form, status, "Legal"))
        v
      case None =>
        Loggers.stateTransitions.warn(formatLogMessage(form, status, "Illegal"))
        form.status
    }

  def apply(from: FormStatus, to: FormStatus): Option[FormStatus] = (from, to) match {
    case (f, t) if t === f                                 => Some(to)
    case (InProgress, _)                                   => Some(to)
    case (NeedsReview, Accepting | Returning | Submitting) => Some(to)
    case (Accepting, Accepted)                             => Some(to)
    case (Returning, InProgress)                           => Some(to)
    case (Accepted, Submitting)                            => Some(to)
    case (Summary, _)                                      => Some(to)
    case (Validated, InProgress)                           => Some(Summary) // This is odd. What's it about?
    case (Validated, Summary | Signed)                     => Some(to)
    case (Signed, Submitted | NeedsReview)                 => Some(to)
    case (Submitting, Submitted | NeedsReview | Accepted)  => Some(to)
    case (Submitted, InProgress)                           => Some(to)
    case _                                                 => None
  }

  private def formatLogMessage(form: Form, to: FormStatus, legality: String) =
    f"$legality%-20s ${form.status}%-20s -> $to%-20s ${form._id.value}"
}
