/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.dms

import java.time.{ Clock, LocalDateTime }
import java.util.Base64

import org.apache.pdfbox.pdmodel.PDDocument
import play.api.mvc.Action
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission._
import uk.gov.hmrc.gform.typeclasses.Rnd

import scala.util.Random

class DmsSubmissionController(
  fileUpload: FileUploadService,
  pdfGenerator: PdfGeneratorService,
  documentLoader: Array[Byte] => PDDocument)(implicit clock: Clock, rnd: Rnd[Random]) extends BaseController {

  def submitToDms = Action.async(parse.json) { implicit request =>
    withJsonBody[DmsHtmlSubmission] { dmsHtmlSubmission =>
      val decodedHtml = decode(dmsHtmlSubmission.html)
      val metadata = dmsHtmlSubmission.metadata
      val formTemplateId = FormTemplateId(metadata.dmsFormId)

      for {
        envId <- fileUpload.createEnvelope(formTemplateId)
        pdf <- pdfGenerator.generatePDF(decodedHtml)
        pdfDoc = documentLoader(pdf)
        pdfSummary = PdfSummary(pdfDoc.getNumberOfPages, pdf)
        _ = pdfDoc.close()
        submissionRef = SubmissionRef.random
        dmsMetadata = DmsMetaData(formTemplateId, metadata.customerId)
        submission = Submission(FormId(metadata.dmsFormId), LocalDateTime.now(clock), submissionRef, envId, 0, dmsMetadata)
        submissionAndPdf = SubmissionAndPdf(submission, pdfSummary)
        dmsSubmission = DmsSubmission(metadata.dmsFormId, TextExpression(Constant(metadata.customerId)), metadata.classificationType, metadata.businessArea)
        _ <- fileUpload.submitEnvelope(submissionAndPdf, dmsSubmission, 0)
      } yield {
        NoContent
      }
    }
  }

  private val decode = (s: String) => new String(Base64.getDecoder.decode(s))
}
