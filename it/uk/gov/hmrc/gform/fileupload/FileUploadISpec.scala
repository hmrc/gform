package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.support.ITSpec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import java.time.LocalDateTime

import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.WSHttp

class FileUploadISpec extends ITSpec {

  "FUService.createEnvelope" - {
    "happy path SHOULD WORK" in new Fixture {
      val headers = Map("Location" -> Seq("localhost:8898/file-upload/envelopes/753bb314-bb61-430f-b812-427ab4cf6da3"))
      val status = 200
      fileUploadConnector.createEnvelope(formTypeId, LocalDateTime.now.plusDays(1)).futureValue shouldBe EnvelopeId(
        "753bb314-bb61-430f-b812-427ab4cf6da3")
    }

    "exceptional situations" in new Fixture {
      val headers = noHeaders
      val status = 200
      val result = fileUploadConnector.createEnvelope(formTypeId, LocalDateTime.now.plusDays(1)).failed.futureValue
      result shouldBe an[SpoiltLocationHeader]
      result.getMessage shouldBe "Header Location not found"
    }

    "fail when spoilt 'Location' header" in new Fixture {
      val headers = Map("Location" -> Seq("spoiltValueHere"))
      val status = 200
      val result = fileUploadConnector.createEnvelope(formTypeId, LocalDateTime.now.plusDays(1)).failed.futureValue
      result shouldBe an[SpoiltLocationHeader]
      result.getMessage shouldBe "spoiltValueHere"
    }

    "fail when 5xx" in new Fixture {
      val headers = noHeaders
      val status = 500
      val result = fileUploadConnector.createEnvelope(formTypeId, LocalDateTime.now.plusDays(1))
      result.failed.futureValue shouldBe an[_root_.uk.gov.hmrc.http.Upstream5xxResponse]
      result.failed.futureValue.getMessage shouldBe "POST of 'http://fileupload.whatever/file-upload/envelopes' returned 500. Response body: 'null'"
    }

    "fail when 4xx" in new Fixture {
      val headers = noHeaders
      val status = 400
      val result = fileUploadConnector.createEnvelope(formTypeId, LocalDateTime.now.plusDays(1))
      result.failed.futureValue shouldBe an[_root_.uk.gov.hmrc.http.BadRequestException]
      result.failed.futureValue.getMessage shouldBe "POST of 'http://fileupload.whatever/file-upload/envelopes' returned 400 (Bad Request). Response body 'null'"
    }
  }

  trait Fixture extends ExampleItFileUploadData {
    val headers: Map[String, Seq[String]]
    val noHeaders = Map.empty[String, Seq[String]]
    val status: Int

    lazy val r = HttpResponse(responseStatus = status, responseHeaders = headers)
    lazy val wSHttp = new StubbedWSHttp(r)

    lazy val fileUploadConnector = new FileUploadConnector(config, wSHttp, FrozenTimeProvider.exampleInstance)
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
    lazy val formTypeId = FormTemplateId("FormId-13-2-3-1233-3")
    lazy val envelopeExpiryDate = LocalDateTime.now.plusDays(1)
  }
}

import java.time.LocalDateTime

class FrozenTimeProvider extends TimeProvider {
  override def localDateTime: LocalDateTime = LocalDateTime.of(2017, 1, 12, 5, 45)
}

object FrozenTimeProvider {
  val exampleInstance: FrozenTimeProvider = new FrozenTimeProvider()
}