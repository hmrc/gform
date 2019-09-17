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

package uk.gov.hmrc.gform.form

import cats.Monad
import cats.instances.list._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.formmetadata.{ FormMetadata, FormMetadataAlgebra }
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.save4later.FormPersistenceAlgebra
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, FormAccessCodeForAgents, FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SubmissionRef, UserId }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

class FormService[F[_]: Monad](
  formPersistence: FormPersistenceAlgebra[F],
  fileUpload: FileUploadAlgebra[F],
  formTemplateAlgebra: FormTemplateAlgebra[F],
  formMetadataAlgebra: FormMetadataAlgebra[F])
    extends FormAlgebra[F] {

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
      val filteredForms: List[(Form, FormMetadata)] = existingForms.flatten.filter {
        case (form, _) => allowedStates.contains(form.status)
      }

      filteredForms.map { case (_, formMetadata) => FormOverview.fromFormMetadata(formMetadata) }
    }

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit] =
    formPersistence.delete(formId)

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
    expiryDays: Long,
    initialFields: Seq[FormField] = Seq.empty)(implicit hc: HeaderCarrier): F[FormIdData] = {
    val timeProvider = new TimeProvider
    val expiryDate = timeProvider.localDateTime().plusDays(expiryDays)

    for {
      envelopeId   <- fileUpload.createEnvelope(formTemplateId, expiryDate)
      formTemplate <- formTemplateAlgebra.get(formTemplateId)
      formIdData = createNewFormData(userId, formTemplate, envelopeId, affinityGroup)
      formId = formIdData.toFormId
      form = Form(
        formId,
        envelopeId,
        userId,
        formTemplateId,
        FormData(fields = initialFields),
        InProgress,
        VisitIndex.empty,
        ThirdPartyData.empty,
        Some(EnvelopeExpiryDate(expiryDate))
      )
      _ <- formPersistence.upsert(formIdData.toFormId, form)
      _ <- formMetadataAlgebra.upsert(formIdData)
    } yield formIdData
  }

  def updateUserData(formIdData: FormIdData, userData: UserData)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      form <- get(formIdData)
      newForm = form
        .copy(
          formData = userData.formData,
          status = newStatus(form, userData.formStatus),
          visitsIndex = userData.visitsIndex,
          thirdPartyData = userData.thirdPartyData
        )
      _ <- formPersistence.upsert(formIdData.toFormId, newForm)
      _ <- refreshMetadata(form.formData != newForm.formData, formIdData, newForm.formData)
    } yield ()

  private def refreshMetadata(toRefresh: Boolean, formIdData: FormIdData, formData: FormData) =
    if (toRefresh) for {
      formTemplate <- formTemplateAlgebra.get(formIdData.formTemplateId)
      parentRefs = resolveParentFormSubmissionRefs(formTemplate.parentFormSubmissionRefs, formData)
      _ <- formMetadataAlgebra.touch(formIdData, parentRefs)
    } yield ()
    else implicitly[Monad[F]].pure(())

  private def resolveParentFormSubmissionRefs(fcIds: List[FormComponentId], formData: FormData): List[SubmissionRef] =
    fcIds.flatMap(fcId => formData.find(fcId).map(SubmissionRef(_)))

  def updateFormStatus(formId: FormId, status: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus] =
    for {
      form <- get(formId)
      newS = newStatus(form, status)
      _ <- formPersistence.upsert(formId, form.copy(status = newS))
    } yield newS

  def forceUpdateFormStatus(formIdData: FormIdData, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    forceUpdateFormStatus(formIdData.toFormId, newStatus)

  def forceUpdateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      form <- get(formId)
      _    <- formPersistence.upsert(formId, form.copy(status = newStatus))
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
    case _                                                 => None
  }

  private def formatLogMessage(form: Form, to: FormStatus, legality: String) =
    f"$legality%-20s ${form.status}%-20s -> $to%-20s ${form._id.value}"
}

object LifeCycleStatusGraphVizRenderer extends App {
  val transientStates = Set(Accepting, Returning, Submitting)

  println("""|digraph stateTransitionTable {
             |  rankdir=LR;
             |  size="8,5"
             |  node[shape = circle];""".stripMargin)
  transientStates.foreach(s => println(s"  $s;"))
  println("  node[shape = doublecircle];")
  println("  InProgress [style=filled; color=green];")
  println("  Submitted [style=filled; color=red];")
  println("  Validated -> Summary [label='InProgress'];")

  for {
    from <- FormStatus.all
    to   <- FormStatus.all
    if from != to
    if LifeCycleStatus(from, to).exists(_ == to)
  } println(s"  $from -> $to;")

  println("}")
}
