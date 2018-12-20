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

import java.time._
import java.util.{ Base64, UUID }

import org.apache.pdfbox.pdmodel.PDDocument
import play.api.libs.json.{ JsValue, Json }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorService
import play.api.test.Helpers._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormTemplateId, TextExpression }
import uk.gov.hmrc.gform.submission._
import uk.gov.hmrc.gform.typeclasses.Rnd
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.util.Random

class DmsSubmissionControllerSpec extends Spec {

  "The DmsSubmissionController" should "create a file upload envelope" in {
    (mockFileUpload
      .createEnvelope(_: FormTemplateId)(_: HeaderCarrier))
      .expects(FormTemplateId(validSubmission.metadata.dmsFormId), *)
      .returning(Future.successful(EnvelopeId("some-envelope-id")))

    (mockPdfGenerator
      .generatePDF(_: String)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(Array.emptyByteArray))

    mockDocumentLoader
      .expects(*)
      .returning(stubPdfDocument)

    (mockFileUpload
      .submitEnvelope(_: SubmissionAndPdf, _: DmsSubmission, _: Int)(_: HeaderCarrier))
      .expects(*, *, *, *)
      .returning(Future.successful(()))

    val res = testController.submitToDms()(validRequest)
    status(res) shouldBe NO_CONTENT
  }

  it should "generate a PDF using the decoded HTML" in {
    val html = "<p>submission</p>"
    val encodedHtml = new String(Base64.getEncoder.encode(html.getBytes))
    val submissionWithHtml = validSubmission.copy(html = encodedHtml)
    val pdfBytes = "this is totally a pdf".getBytes

    (mockFileUpload
      .createEnvelope(_: FormTemplateId)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(EnvelopeId("some-envelope-id")))

    (mockPdfGenerator
      .generatePDF(_: String)(_: HeaderCarrier))
      .expects(html, *)
      .returning(Future.successful(pdfBytes))

    mockDocumentLoader
      .expects(*)
      .returning(stubPdfDocument)

    (mockFileUpload
      .submitEnvelope(_: SubmissionAndPdf, _: DmsSubmission, _: Int)(_: HeaderCarrier))
      .expects(*, *, *, *)
      .returning(Future.successful(()))

    val res = testController.submitToDms()(FakeRequest().withBody[JsValue](Json.toJson(submissionWithHtml)))
    status(res) shouldBe NO_CONTENT
  }

  it should "upload the PDF and XML metadata to the file upload envelope" in {
    val submissionRef = SubmissionRef.random(fixedRnd)

    val numberOfPages = 1
    val pdfContent = "totally a pdf".getBytes
    val expectedEnvId = EnvelopeId(UUID.randomUUID().toString)
    val dmsMetadata =
      DmsMetaData(FormTemplateId(validSubmission.metadata.dmsFormId), validSubmission.metadata.customerId)
    val expectedSubmissionAndPdf = SubmissionAndPdf(
      Submission(FormId(validSubmission.metadata.dmsFormId), fixedTime, submissionRef, expectedEnvId, 0, dmsMetadata),
      PdfSummary(numberOfPages.longValue, pdfContent)
    )

    val expectedDmsSubmission = DmsSubmission(
      validSubmission.metadata.dmsFormId,
      TextExpression(Constant(validSubmission.metadata.customerId)),
      validSubmission.metadata.classificationType,
      validSubmission.metadata.businessArea
    )

    (mockFileUpload
      .createEnvelope(_: FormTemplateId)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(expectedEnvId))

    (mockPdfGenerator
      .generatePDF(_: String)(_: HeaderCarrier))
      .expects(*, *)
      .returning(Future.successful(pdfContent))

    mockDocumentLoader
      .expects(*)
      .returning(stubPdfDocument)

    (stubPdfDocument.getNumberOfPages _).when().returning(numberOfPages)

    (mockFileUpload
      .submitEnvelope(_: SubmissionAndPdf, _: DmsSubmission, _: Int)(_: HeaderCarrier))
      .expects(expectedSubmissionAndPdf, expectedDmsSubmission, *, *)
      .returning(Future.successful(()))

    val res = testController.submitToDms()(validRequest)
    status(res) shouldBe NO_CONTENT
  }

  it should "return a 400 Bad Request response when the JSON payload is invalid" in {
    val res = testController.submitToDms()(FakeRequest().withBody[JsValue](Json.obj()))
    status(res) shouldBe BAD_REQUEST
  }

  lazy val validSubmission = DmsHtmlSubmission("", DmsMetadata("some-form-id", "some-customer-id", "", ""))

  lazy val validPayload = Json.toJson(validSubmission)
  lazy val validRequest = FakeRequest().withBody[JsValue](validPayload)

  lazy val mockFileUpload = mock[FileUploadService]
  lazy val mockPdfGenerator = mock[PdfGeneratorService]
  lazy val mockDocumentLoader = mockFunction[Array[Byte], PDDocument]
  lazy val stubPdfDocument = stub[PDDocument]
  lazy val fixedTime = LocalDateTime.of(2018, 3, 2, 0, 0)
  lazy val fixedRnd = new Rnd[Random] {
    val notVeryRandom = stub[Random]
    (notVeryRandom.nextInt(_: Int)).when(*).returning(4)

    override def apply() = notVeryRandom
  }

  lazy val testController = new DmsSubmissionController(mockFileUpload, mockPdfGenerator, mockDocumentLoader)(
    Clock.fixed(fixedTime.toInstant(ZoneOffset.UTC), ZoneId.systemDefault),
    fixedRnd)
}
