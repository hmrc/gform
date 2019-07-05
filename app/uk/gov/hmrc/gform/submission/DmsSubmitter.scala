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
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA, fromOptA }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormTemplate }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait DmsSubmitter[F[_]] {
  def apply(submissionInfo: DestinationSubmissionInfo, dmsSubmission: DmsSubmission)(
    implicit hc: HeaderCarrier): F[Unit]
}

class FileUploadServiceDmsSubmitter(
  fileUploadService: FileUploadService,
  formService: FormAlgebra[FOpt],
  formTemplateService: FormTemplateAlgebra[FOpt],
  submissionRepo: SubmissionRepo,
  pdfGeneratorService: PdfGeneratorService,
  timeProvider: TimeProvider)(implicit ec: ExecutionContext)
    extends DmsSubmitter[FOpt] {
  def apply(submissionInfo: DestinationSubmissionInfo, dmsSubmission: DmsSubmission)(
    implicit hc: HeaderCarrier): FOpt[Unit] = {
    import submissionInfo._
    import submissionInfo.submissionData._

    for {
      form         <- formService.get(formId)
      formTemplate <- formTemplateService.get(form.formTemplateId)
      submission = createSubmission(form, customerId, formTemplate)
      _                 <- submissionRepo.upsert(submission)
      sectionFormFields <- fromOptA(SubmissionServiceHelper.getSectionFormFields(form, formTemplate, affinityGroup))
      summaries <- fromFutureA(
                    PdfAndXmlSummariesFactory
                      .withPdf(pdfGeneratorService, pdfData)
                      .apply(
                        form,
                        formTemplate,
                        structuredFormData,
                        sectionFormFields,
                        customerId,
                        submission.submissionRef,
                        dmsSubmission))
      numberOfAttachments = sectionFormFields.map(_.numberOfFiles()).sum
      res <- fromFutureA(fileUploadService.submitEnvelope(submission, summaries, dmsSubmission, numberOfAttachments))
      _   <- formService.updateFormStatus(submissionInfo.formId, Submitted)
    } yield res
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
      submissionRef = SubmissionRef(form.seed.value),
      envelopeId = form.envelopeId,
      _id = form._id,
      noOfAttachments = getNoOfAttachments(form, formTemplate),
      dmsMetaData = DmsMetaData(
        formTemplateId = form.formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )

}
