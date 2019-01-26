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
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.email.EmailService
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.FormTemplateService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiSubmitter
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionService(
  pdfGeneratorService: PdfGeneratorService,
  formService: FormService,
  formTemplateService: FormTemplateService,
  fileUploadService: FileUploadService,
  handlebarsApiHttpSubmitter: HandlebarsHttpApiSubmitter[FOpt],
  submissionRepo: SubmissionRepo,
  timeProvider: TimeProvider,
  email: EmailService) {

  def submissionWithoutPdf(formId: FormId, customerId: String, affinityGroup: Option[AffinityGroup])(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      form                <- fromFutureA        (getSignedForm(formId))
      res                 <-                    submission(form, customerId, affinityGroup, PdfAndXmlSummariesFactory.withoutPdf(pdfGeneratorService))
    } yield res
  // format: ON

  def submissionWithPdf(formId: FormId, customerId: String, affinityGroup: Option[AffinityGroup], pdf: String)(
    implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      form                <- fromFutureA        (formService.get(formId))
      res                 <-                    submission(form, customerId, affinityGroup, PdfAndXmlSummariesFactory.withPdf(pdfGeneratorService, pdf))
    } yield res
  // format: ON

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
      submissionRef = SubmissionRef.random,
      envelopeId = form.envelopeId,
      _id = form._id,
      noOfAttachments = getNoOfAttachments(form, formTemplate),
      dmsMetaData = DmsMetaData(
        formTemplateId = form.formTemplateId,
        customerId //TODO need more secure and safe way of doing this. perhaps moving auth to backend and just pulling value out there.
      )
    )

  private def getSignedForm(formId: FormId)(implicit hc: HeaderCarrier) = formService.get(formId).flatMap {
    case f @ Form(_, _, _, _, _, _, Signed) => Future.successful(f)
    case _                                  => Future.failed(new Exception(s"Form $FormId status is not signed"))
  }

  private def submission(
    form: Form,
    customerId: String,
    affinityGroup: Option[AffinityGroup],
    pdfAndXmlSummaryFactory: PdfAndXmlSummariesFactory)(implicit hc: HeaderCarrier): FOpt[Unit] =
    // format: OFF
    for {
      _                     <- fromFutureA          (formService.updateUserData(form._id, UserData(form.formData, form.repeatingGroupStructure, Submitted)))
      formTemplate          <- fromFutureA          (formTemplateService.get(form.formTemplateId))
      submission            =                       createSubmission(form, customerId, formTemplate)
      _                     <-                      submissionRepo.upsert(submission)
      destinationsSubmitter =                       new DestinationsSubmitter(new FileUploadServiceDmsSubmitter(fileUploadService), handlebarsApiHttpSubmitter)
      res                   <-                      destinationsSubmitter.send(DestinationSubmissionInfo(submission, form, formTemplate, customerId, affinityGroup, pdfAndXmlSummaryFactory))
      emailAddress          =                       email.getEmailAddress(form)
      _                     <-                      fromFutureA(email.sendEmail(emailAddress, formTemplate.emailTemplateId)(hc, fromLoggingDetails))
    } yield res
  // format: ON

  def submissionDetails(formId: FormId)(implicit ec: ExecutionContext): Future[Submission] =
    submissionRepo.get(formId.value)
}
