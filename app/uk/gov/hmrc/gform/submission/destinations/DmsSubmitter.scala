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

package uk.gov.hmrc.gform.submission.destinations

import java.time.Instant

import cats.instances.future._
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA, success }
import uk.gov.hmrc.gform.fileupload.{ File, FileUploadService }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.PdfHtml
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.PdfAndXmlSummariesFactory
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

class DmsSubmitter(
  fileUploadService: FileUploadService,
  formService: FormAlgebra[FOpt],
  formTemplateService: FormTemplateAlgebra[FOpt],
  pdfGeneratorService: PdfGeneratorService)(implicit ec: ExecutionContext)
    extends DmsSubmitterAlgebra[FOpt] {
  private val logger = LoggerFactory.getLogger(getClass)

  def apply(
    submissionInfo: DestinationSubmissionInfo,
    pdfData: PdfHtml,
    instructionPdfData: Option[PdfHtml],
    structuredFormData: StructuredFormValue.ObjectStructure,
    hmrcDms: HmrcDms)(implicit hc: HeaderCarrier): FOpt[Unit] = {
    import submissionInfo._
    implicit val now: Instant = Instant.now()
    for {
      form         <- formService.get(formId)
      formTemplate <- formTemplateService.get(form.formTemplateId)
      summaries <- fromFutureA(
                    PdfAndXmlSummariesFactory
                      .withPdf(pdfGeneratorService, pdfData, instructionPdfData)
                      .apply(form, formTemplate, structuredFormData, customerId, submission.submissionRef, hmrcDms))
      res             <- fromFutureA(fileUploadService.submitEnvelope(submission, summaries, hmrcDms))
      envelopeDetails <- fromFutureA(fileUploadService.getEnvelope(submission.envelopeId))
      _               <- success(logFileSizeBreach(submission.envelopeId, envelopeDetails.files))
      _               <- formService.updateFormStatus(submissionInfo.formId, Submitted)
    } yield res
  }

  /**
    * This log line is used in alert-config, to trigger a pager duty alert when files sizes exceeds the threshold
    * value (currently 50 MB)
    *
    *  1 MB = 1000000 Bytes in base 10 system
    * @param files
    */
  private def logFileSizeBreach(envelopeId: EnvelopeId, files: List[File]) = {
    val totalFileSize = files.map(_.length).sum
    val totalFileSizeMB = Math.ceil(totalFileSize / 1000000.toDouble)
    val thresholdMB = 50
    logger.info(
      s"GForm DMS submission attachments size is $totalFileSize B (rounded to $totalFileSizeMB MB) [envelopeId=${envelopeId.value}]")
    if (totalFileSizeMB > thresholdMB) {
      logger.warn(s"GForm DMS submission attachments size exceeds $thresholdMB MB [envelopeId=${envelopeId.value}]")
    }
  }
}
