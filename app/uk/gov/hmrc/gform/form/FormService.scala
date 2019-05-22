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
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.flatMap._
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.save4later.FormPersistenceAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

class FormService[F[_]: Monad](formPersistence: FormPersistenceAlgebra[F], fileUpload: FileUploadAlgebra[F])
    extends FormAlgebra[F] {

  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form] =
    formPersistence.get(formId)

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit] =
    formPersistence.delete(formId)

  def create(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode],
    expiryDays: Long,
    initialFields: Seq[FormField] = Seq.empty)(implicit hc: HeaderCarrier): F[FormId] = {
    val timeProvider = new TimeProvider
    val formId = FormId(userId, formTemplateId, accessCode)
    val expiryDate = timeProvider.localDateTime().plusDays(expiryDays)

    for {
      envelopeId <- fileUpload.createEnvelope(formTemplateId, expiryDate)
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
      _ <- formPersistence.upsert(formId, form)
    } yield formId
  }

  def updateUserData(formId: FormId, userData: UserData)(implicit hc: HeaderCarrier): F[Unit] =
    for {
      form <- get(formId)
      newForm = form
        .copy(
          formData = userData.formData,
          status = newStatus(form, userData.formStatus),
          visitsIndex = userData.visitsIndex,
          thirdPartyData = userData.thirdPartyData
        )
      _ <- formPersistence.upsert(formId, newForm)
    } yield ()

  def updateFormStatus(formId: FormId, status: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus] =
    for {
      form <- get(formId)
      newS = newStatus(form, status)
      _ <- formPersistence.upsert(formId, form.copy(status = newS))
    } yield newS

  private def newStatus(form: Form, status: FormStatus) =
    LifeCycleStatus.newStatus(form, status)

  def updateDestinationSubmissionInfo(formId: FormId, info: Option[DestinationSubmissionInfo])(
    implicit hc: HeaderCarrier): F[Unit] =
    for {
      form <- get(formId)
      _    <- formPersistence.upsert(formId, form.copy(destinationSubmissionInfo = info))
    } yield ()

  def saveKeyStore(formId: FormId, data: Map[String, JsValue])(implicit hc: HeaderCarrier): F[Unit] =
    formPersistence.saveKeyStore(formId, data)

  def getKeyStore(formId: FormId)(implicit hc: HeaderCarrier): F[Option[Map[String, JsValue]]] =
    formPersistence.getKeyStore(formId)
}

object LifeCycleStatus {
  def newStatus(form: Form, status: FormStatus): FormStatus = (form.status, status) match {
    case (InProgress, _)                      => status
    case (NeedsReview, Approved | InProgress) => status
    case (NeedsReview, _)                     => form.status
    case (Approved, Submitted)                => status
    case (Approved, _)                        => form.status
    case (Summary, InProgress)                => form.status
    case (Summary, _)                         => status
    case (Validated, InProgress)              => Summary
    case (Validated, Summary | Signed)        => status
    case (Validated, _)                       => form.status
    case (Signed, Submitted | NeedsReview)    => status
    case (Signed, _)                          => form.status
    case (Submitted, _)                       => form.status
  }
}
