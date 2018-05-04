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

package uk.gov.hmrc.gform
package fileupload

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.FrozenTimeProvider
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class FileUploadConnectorSpec extends Spec {

  behavior of "FUService.createEnvelope - happy path"

  it should "work" in new Fixture {
    val headers = Map("Location" -> Seq("localhost:8898/file-upload/envelopes/753bb314-bb61-430f-b812-427ab4cf6da3"))
    val status = 200
    fileUploadConnector.createEnvelope(formTypeId).futureValue shouldBe EnvelopeId(
      "753bb314-bb61-430f-b812-427ab4cf6da3")
  }

  behavior of "FUService.createEnvelope - exceptional situations"

  it should "fail when no 'Location' header" in new Fixture {
    val headers = noHeaders
    val status = 200
    val result = fileUploadConnector.createEnvelope(formTypeId).failed.futureValue
    result shouldBe an[SpoiltLocationHeader]
    result.getMessage shouldBe "Header Location not found"
  }

  it should "fail when spoilt 'Location' header" in new Fixture {
    val headers = Map("Location" -> Seq("spoiltValueHere"))
    val status = 200
    val result = fileUploadConnector.createEnvelope(formTypeId).failed.futureValue
    result shouldBe an[SpoiltLocationHeader]
    result.getMessage shouldBe "spoiltValueHere"
  }

  it should "fail when 5xx" in new Fixture {
    val headers = noHeaders
    val status = 500
    val result = fileUploadConnector.createEnvelope(formTypeId)
    result.failed.futureValue shouldBe an[_root_.uk.gov.hmrc.http.Upstream5xxResponse]
    result.failed.futureValue.getMessage shouldBe "POST of 'http://fileupload.whatever/file-upload/envelopes' returned 500. Response body: 'null'"
  }

  it should "fail when 4xx" in new Fixture {
    val headers = noHeaders
    val status = 400
    val result = fileUploadConnector.createEnvelope(formTypeId)
    result.failed.futureValue shouldBe an[_root_.uk.gov.hmrc.http.BadRequestException]
    result.failed.futureValue.getMessage shouldBe "POST of 'http://fileupload.whatever/file-upload/envelopes' returned 400 (Bad Request). Response body 'null'"
  }

  trait Fixture extends ExampleFileUploadData {

    val headers: Map[String, Seq[String]]
    val noHeaders = Map.empty[String, Seq[String]]
    val status: Int

    lazy val r = HttpResponse(responseStatus = status, responseHeaders = headers)
    lazy val wSHttp = new StubbedWSHttp(r)

    lazy val fileUploadConnector = new FileUploadConnector(config, wSHttp, FrozenTimeProvider.exampleInstance)
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
    lazy val formTypeId = FormTemplateId("FormId-13-2-3-1233-3")
  }

}
