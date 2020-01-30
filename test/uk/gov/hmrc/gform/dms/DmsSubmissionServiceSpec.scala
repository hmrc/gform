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

package uk.gov.hmrc.gform.dms

import java.time._
import java.util.UUID

import cats.Id
import org.apache.pdfbox.pdmodel.PDDocument
import org.scalamock.function.MockFunction1
import play.api.libs.Files.{ SingletonTemporaryFileCreator, TemporaryFile }
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.MultipartFormData
import play.api.mvc.MultipartFormData.{ BadPart, FilePart }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.{ Attachments, FileUploadAlgebra }
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.submission._
import uk.gov.hmrc.http.HeaderCarrier

class DmsSubmissionServiceSpec extends Spec {
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "submitToDms" should "create the correct uploadable things and do the submission" in {

    val numberOfPages = 1
    val pdfContent = "totally a pdf".getBytes
    val expectedEnvId = EnvelopeId(UUID.randomUUID().toString)
    val expectedPdfAndXmlSummaries = PdfAndXmlSummaries(PdfSummary(numberOfPages.longValue, pdfContent))

    val expectedDmsSubmission = DmsSubmissionService.createDmsSubmission(validSubmission.metadata)
    val expectedSubmission =
      DmsSubmissionService.createSubmission(validSubmission.metadata, expectedEnvId, fixedTime, Attachments.empty)

    (stubPdfDocument.getNumberOfPages _).when().returning(numberOfPages)

    fixture
      .expectCreateEnvelope(FormTemplateId(validSubmission.metadata.dmsFormId), expectedEnvId)
      .expectGeneratePdfBytes(validSubmission.b64html, pdfContent)
      .expectLoadDocument(pdfContent, stubPdfDocument)
      .expectSubmitEnvelope(expectedSubmission, expectedPdfAndXmlSummaries, expectedDmsSubmission, 0)
      .service
      .submitToDms(validSubmission) shouldBe expectedEnvId
  }

  "submitPdfToDms" should "create the correct uploadable things and do the submission" in {
    val numberOfPages = 1
    val expectedEnvId = EnvelopeId(UUID.randomUUID().toString)
    val pdfContent = "totally a pdf".getBytes
    val expectedPdfAndXmlSummaries = PdfAndXmlSummaries(PdfSummary(numberOfPages.longValue, pdfContent))

    val expectedDmsSubmission = DmsSubmissionService.createDmsSubmission(validSubmission.metadata)
    val expectedSubmission =
      DmsSubmissionService.createSubmission(validSubmission.metadata, expectedEnvId, fixedTime, Attachments.empty)

    (stubPdfDocument.getNumberOfPages _).when().returning(numberOfPages)

    val tmpPdf: TemporaryFile = SingletonTemporaryFileCreator.create("agents", ".pdf")
    tmpPdf.deleteOnExit()

    fixture
      .expectCreateEnvelope(FormTemplateId(validSubmission.metadata.dmsFormId), expectedEnvId)
      .expectLoadDocument(pdfContent, stubPdfDocument)
      .expectSubmitEnvelope(expectedSubmission, expectedPdfAndXmlSummaries, expectedDmsSubmission, 0)
      .service
      .submitPdfToDms(pdfContent, validSubmission.metadata) shouldBe expectedEnvId
  }

  private val validSubmission = DmsHtmlSubmission("", DmsMetadata("some-form-id", "some-customer-id", "", ""))
  private val stubPdfDocument = stub[PDDocument]

  private val envelopeExpiryDays: Long = 5L
  private val fixedTime = LocalDateTime.of(2018, 3, 2, 0, 0)

  case class Fixture(
    service: DmsSubmissionService[Id],
    fileUpload: FileUploadAlgebra[Id],
    pdfGenerator: PdfGeneratorAlgebra[Id],
    documentLoader: MockFunction1[Array[Byte], PDDocument],
    clock: Clock) {
    def expectCreateEnvelope(formTemplateId: FormTemplateId, envelopeId: EnvelopeId): Fixture = {
      (fileUpload
        .createEnvelope(_: FormTemplateId, _: LocalDateTime)(_: HeaderCarrier))
        .expects(FormTemplateId(validSubmission.metadata.dmsFormId), fixedTime.plusDays(envelopeExpiryDays), hc)
        .returning(envelopeId)

      this
    }

    def expectGeneratePdfBytes(pdfHtml: String, pdfBytes: Array[Byte]): Fixture = {
      (pdfGenerator
        .generatePDFBytes(_: String)(_: HeaderCarrier))
        .expects(pdfHtml, hc)
        .returning(pdfBytes)

      this
    }

    def expectLoadDocument(pdfBytes: Array[Byte], document: PDDocument): Fixture = {
      documentLoader
        .expects(pdfBytes)
        .returning(document)

      this
    }

    def expectSubmitEnvelope(
      submission: Submission,
      pdfAndXmlSummaries: PdfAndXmlSummaries,
      dmsSubmission: DmsSubmission,
      numberOfAttachments: Int): Fixture = {
      (fileUpload
        .submitEnvelope(_: Submission, _: PdfAndXmlSummaries, _: DmsSubmission)(_: HeaderCarrier))
        .expects(submission, pdfAndXmlSummaries, dmsSubmission, hc)
        .returning(())

      this
    }
  }

  def fixture(): Fixture = {
    val fileUpload = mock[FileUploadAlgebra[Id]]
    val pdfGenerator = mock[PdfGeneratorAlgebra[Id]]
    val documentLoader = mockFunction[Array[Byte], PDDocument]
    implicit val clock = Clock.fixed(fixedTime.toInstant(ZoneOffset.UTC), ZoneId.systemDefault)

    Fixture(
      new DmsSubmissionService(fileUpload, pdfGenerator, documentLoader, envelopeExpiryDays),
      fileUpload,
      pdfGenerator,
      documentLoader,
      clock
    )
  }
}
