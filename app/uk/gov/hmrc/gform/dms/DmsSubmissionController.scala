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

package uk.gov.hmrc.gform.dms

import java.nio.file.Paths
import java.time.{ Clock, LocalDateTime }
import java.util.Base64

import org.apache.pdfbox.pdmodel.PDDocument
import play.api.Logger
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{ JsError, JsSuccess, JsValue, Json }
import play.api.mvc.{ Action, MultipartFormData }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.submission._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class DmsSubmissionController(
  fileUpload: FileUploadService,
  pdfGenerator: PdfGeneratorService,
  documentLoader: Array[Byte] => PDDocument,
  config: AppConfig)(implicit clock: Clock, ex: ExecutionContext)
    extends BaseController {

  def submitToDms: Action[JsValue] = Action.async(parse.json) { implicit request =>
    withJsonBody[DmsHtmlSubmission] { dmsHtmlSubmission =>
      val decodedHtml = decode(dmsHtmlSubmission.html)
      val metadata = dmsHtmlSubmission.metadata

      pdfGenerator.generatePDF(decodedHtml).flatMap { byteArray =>
        submit(byteArray, metadata)
      }
    }
  }

  /**
    * This endpoint is not used by gform but its here for other services to leverage our backend integration with DMS via FUaaS
    * Its currently used by Overseas Agents team
    */
  def submitPdfToDms: Action[MultipartFormData[Files.TemporaryFile]] = Action.async(parse.multipartFormData) {
    implicit request =>
      def validContentType(filePart: MultipartFormData.FilePart[TemporaryFile]) =
        filePart.contentType.map(_.toLowerCase).contains("application/pdf")

      request.body.files.headOption match {
        case Some(file) if validContentType(file) =>
          val dataParts = request.body.dataParts.mapValues(_.mkString(""))
          Json.toJson(dataParts).validate[DmsMetadata] match {
            case JsSuccess(metadata, _) =>
              val file = request.body.files.head.ref.file.getAbsolutePath
              val byteArray = java.nio.file.Files.readAllBytes(Paths.get(file))
              submit(byteArray, metadata)
            case JsError(errors) =>
              Logger.info(s"could not parse DmsMetadata from the request, errors: $errors")
              Future.successful(BadRequest("invalid metadata in the request"))
          }
        case _ =>
          Logger.info("request should contain a pdf file with Content-Type:'application/pdf'")
          Future.successful(BadRequest("request should contain a pdf file with Content-Type:'application/pdf'"))
      }
  }

  private def submit(byteArray: Array[Byte], metadata: DmsMetadata)(implicit hc: HeaderCarrier) = {
    val formTemplateId = FormTemplateId(metadata.dmsFormId)
    for {
      envId <- fileUpload.createEnvelope(formTemplateId, LocalDateTime.now(clock).plusDays(config.formExpiryDays))
      pdfDoc = documentLoader(byteArray)
      pdfSummary = PdfSummary(pdfDoc.getNumberOfPages.toLong, byteArray)
      _ = pdfDoc.close()
      submissionRef = SubmissionRef(envId)
      dmsMetadata = DmsMetaData(formTemplateId, metadata.customerId)
      submission = Submission(
        FormId(metadata.dmsFormId),
        LocalDateTime.now(clock),
        submissionRef,
        envId,
        0,
        dmsMetadata)
      // TODO controller should allow data XML submission
      summaries = PdfAndXmlSummaries(pdfSummary)
      dmsSubmission = DmsSubmission(
        metadata.dmsFormId,
        TextExpression(Constant(metadata.customerId)),
        metadata.classificationType,
        metadata.businessArea)
      _ <- fileUpload.submitEnvelope(submission, summaries, dmsSubmission, 0)
    } yield {
      NoContent
    }
  }

  private val decode = (s: String) => new String(Base64.getDecoder.decode(s))
}
