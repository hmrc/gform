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

package uk.gov.hmrc.gform.submission

import cats.instances.future._
import play.api.Logger
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.SubmissionData
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormTemplate }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  pdfGeneratorService: PdfGeneratorService,
  formAlgebra: FormAlgebra[FOpt],
  formTemplateService: FormTemplateService,
  destinationsSubmitter: DestinationsSubmitter[FOpt],
  submissionRepo: SubmissionRepo,
  email: EmailService,
  timeProvider: TimeProvider)(implicit ex: ExecutionContext) {

  def submissionWithPdf(
    formId: FormId,
    customerId: String,
    affinityGroup: Option[AffinityGroup],
    submissionData: SubmissionData)(implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
      for {
        form          <- formAlgebra.get(formId)
        formTemplate  <- fromFutureA(formTemplateService.get(form.formTemplateId))
        submission    <- findOrCreateSubmission(form, customerId, formTemplate)
        submissionInfo = DestinationSubmissionInfo(formId, customerId, affinityGroup, submissionData, submission)
        _             <- destinationsSubmitter.send(submissionInfo, formTemplate, formAlgebra)
        emailAddress   = email.getEmailAddress(form)
        _             <- fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId, submissionData.emailParameters))
      } yield ()
      // format: ON

  def submissionDetails(formId: FormId)(implicit ex: ExecutionContext): Future[Submission] =
    submissionRepo.get(formId.value)

  private def findOrCreateSubmission(form: Form, customerId: String, formTemplate: FormTemplate): FOpt[Submission] =
    fromFutureA(submissionRepo.find(form._id.value))
      .flatMap(_.fold(insertSubmission(form, customerId, formTemplate)) { s =>
        Logger.info(s"Found Submission for ${form._id.value}")
        uk.gov.hmrc.gform.core.success(s)
      })

  private def insertSubmission(form: Form, customerId: String, formTemplate: FormTemplate): FOpt[Submission] = {
    val submission = createSubmission(form, customerId, formTemplate)
    submissionRepo
      .upsert(submission)
      .map(_ => Logger.info(s"Inserted Submission for ${form._id.value}"))
      .map(_ => submission)
  }

  private def getNoOfAttachments(form: Form, formTemplate: FormTemplate): Int = {
    // TODO two functions are calculating the same thing in different ways! c.f. FileUploadService.SectionFormField.getNumberOfFiles
    val attachmentsIds: List[String] =
      formTemplate.sections.flatMap(_.fields.filter(f => f.`type` == FileUpload())).map(_.id.value)
    val formIds: Seq[String] = form.formData.fields.filterNot(_.value == FileUploadField.noFileUpload).map(_.id.value)
    attachmentsIds.count(ai => formIds.contains(ai))
  }

  private def createSubmission(form: Form, customerId: String, formTemplate: FormTemplate) =
    Submission(
      submittedDate = timeProvider.localDateTime(),
      submissionRef = SubmissionRef(form.envelopeId),
      envelopeId = form.envelopeId,
      _id = form._id,
      noOfAttachments = getNoOfAttachments(form, formTemplate),
      dmsMetaData = DmsMetaData(
        formTemplateId = form.formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )
}
