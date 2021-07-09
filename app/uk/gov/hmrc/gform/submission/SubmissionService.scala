/*
 * Copyright 2021 HM Revenue & Customs
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
import org.mongodb.scala.model.Filters.equal
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core.{ fromFutureA, _ }
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
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
  timeProvider: TimeProvider
)(implicit ex: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(getClass)

  def createSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  ): FOpt[Submission] = {
    val submission = buildSubmission(formId, formTemplateId, envelopeId, customerId, noOfAttachments)
    submissionRepo
      .upsert(submission)
      .map(_ => logger.info(s"Upserted Submission for ${formId.value}"))
      .map(_ => submission)
  }

  def submitForm(formIdData: FormIdData, customerId: String, submissionData: SubmissionData)(implicit
    hc: HeaderCarrier
  ): FOpt[Unit] =
    // format: OFF
      for {
        form          <- formAlgebra.get(formIdData)
        formTemplate  <- fromFutureA(formTemplateService.get(form.formTemplateId))
        submission    <- findSubmission(SubmissionId(formIdData.toFormId, form.envelopeId))
        submissionInfo = DestinationSubmissionInfo(customerId, submission)
        modelTree     <- createModelTreeForSingleFormSubmission(form, formTemplate, submissionData, submission.submissionRef)
        _             <- destinationsSubmitter.send(submissionInfo, modelTree, Some(form.formData), submissionData.l)
        emailAddress   = email.getEmailAddress(form)
        _             <- fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId, submissionData.emailParameters))
      } yield ()
  // format: ON

  private def createModelTreeForSingleFormSubmission(
    form: Form,
    formTemplate: FormTemplate,
    submissionData: SubmissionData,
    submissionRef: SubmissionRef
  )(implicit hc: HeaderCarrier) =
    destinationProcessorModelService
      .create(
        form,
        submissionData.variables,
        submissionData.pdfData,
        submissionData.instructionPDFData,
        submissionData.structuredFormData
      )
      .map(model =>
        HandlebarsModelTree(
          form._id,
          submissionRef,
          formTemplate,
          submissionData.pdfData,
          submissionData.instructionPDFData,
          submissionData.structuredFormData,
          model
        )
      )

  def submissionDetails(submissionId: SubmissionId): Future[Submission] =
    submissionRepo.get(submissionId.idString)

  private def findSubmission(submissionId: SubmissionId) =
    fromFutureA(submissionRepo.get(submissionId.idString))

  private def buildSubmission(
    formId: FormId,
    formTemplateId: FormTemplateId,
    envelopeId: EnvelopeId,
    customerId: String,
    noOfAttachments: Int
  ) =
    Submission(
      submittedDate = timeProvider.localDateTime(),
      submissionRef = SubmissionRef(envelopeId),
      envelopeId = envelopeId,
      _id = SubmissionId(formId, envelopeId),
      noOfAttachments = noOfAttachments,
      dmsMetaData = DmsMetaData(
        formTemplateId = formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )

  def submissionPageDetails(formTemplateId: FormTemplateId, page: Int, pageSize: Int): Future[SubmissionPageData] = {
    val query = equal("formTemplateId", formTemplateId.value)
    val sort = equal("submittedDate", -1)

    val skip = page * pageSize
    for {
      submissions <- submissionRepo.page(query, sort, skip, pageSize)
      count       <- submissionRepo.count(query)
    } yield SubmissionPageData(submissions, count)
  }
}
