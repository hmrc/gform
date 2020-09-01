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

package uk.gov.hmrc.gform.submission

import cats.instances.future._
import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core.{ fromFutureA, _ }
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.fileupload.Attachments
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormTemplate }
import uk.gov.hmrc.gform.submission.destinations.{ DestinationSubmissionInfo, DestinationsProcessorModelAlgebra, DestinationsSubmitterAlgebra }
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  formAlgebra: FormAlgebra[FOpt],
  formTemplateService: FormTemplateAlgebra[Future],
  destinationProcessorModelService: DestinationsProcessorModelAlgebra[FOpt],
  destinationsSubmitter: DestinationsSubmitterAlgebra[FOpt],
  submissionRepo: Repo[Submission],
  email: EmailService,
  timeProvider: TimeProvider)(implicit ex: ExecutionContext) {

  def submitForm(formIdData: FormIdData, customerId: String, submissionData: SubmissionData)(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
      for {
        form          <- formAlgebra.get(formIdData)
        formTemplate  <- fromFutureA(formTemplateService.get(form.formTemplateId))
        submission    <- findOrCreateSubmission(form, customerId, formTemplate, submissionData.attachments)
        submissionInfo = DestinationSubmissionInfo(customerId, submission)
        modelTree     <- createModelTreeForSingleFormSubmission(form, formTemplate, submissionData, submission.submissionRef)
        _             <- destinationsSubmitter.send(submissionInfo, modelTree, Some(form))
        emailAddress   = email.getEmailAddress(form)
        _             <- fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId, submissionData.emailParameters))
      } yield ()
  // format: ON

  private def createModelTreeForSingleFormSubmission(
    form: Form,
    formTemplate: FormTemplate,
    submissionData: SubmissionData,
    submissionRef: SubmissionRef)(implicit hc: HeaderCarrier) =
    destinationProcessorModelService
      .create(form, submissionData.variables, submissionData.pdfData, submissionData.structuredFormData)
      .map(
        model =>
          HandlebarsModelTree(
            form._id,
            submissionRef,
            formTemplate,
            submissionData.pdfData,
            submissionData.structuredFormData,
            model))

  def submissionDetails(formIdData: FormIdData): Future[Submission] =
    submissionRepo.get(formIdData.toFormId.value)

  private def findOrCreateSubmission(
    form: Form,
    customerId: String,
    formTemplate: FormTemplate,
    attachments: Attachments): FOpt[Submission] =
    form.status match {
      case NeedsReview | Accepting | Returning | Accepted | Submitting | Submitted => findSubmission(form._id)
      case _                                                                       => insertSubmission(form, customerId, formTemplate, attachments)
    }

  private def findSubmission(formId: FormId) =
    fromFutureA(submissionRepo.get(formId.value))

  private def insertSubmission(
    form: Form,
    customerId: String,
    formTemplate: FormTemplate,
    attachments: Attachments): FOpt[Submission] = {
    val submission = createSubmission(form, customerId, formTemplate, attachments)
    submissionRepo
      .upsert(submission)
      .map(_ => Logger.info(s"Upserted Submission for ${form._id.value}"))
      .map(_ => submission)
  }

  private def createSubmission(form: Form, customerId: String, formTemplate: FormTemplate, attachments: Attachments) =
    Submission(
      submittedDate = timeProvider.localDateTime(),
      submissionRef = SubmissionRef(form.envelopeId),
      envelopeId = form.envelopeId,
      _id = form._id,
      noOfAttachments = attachments.size,
      dmsMetaData = DmsMetaData(
        formTemplateId = form.formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )

  def submissionDetailsAll(formTemplateId: FormTemplateId): Future[List[Submission]] = {
    val query = Json.obj(
      "formTemplateId" -> formTemplateId.value
    )
    submissionRepo.search(query)
  }
}
