/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileUpload

import java.nio.file.{ Files, Paths }

import akka.util.ByteString
import uk.gov.hmrc.gform.models.FormTypeId
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.TestWSHttp
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object DemoApp extends App {

  val config = Config(
    fileUploadBaseUrl = "http://localhost:8898",
    fileUploadFrontendBaseUrl = "http://localhost:8899",
    expiryDays = 30,
    maxSize = "20MB",
    maxSizePerItem = "5MB",
    maxItems = 3
  )

  val http = TestWSHttp

  val time = new TimeModule {}
  val fu = new FileUploadConnector(config, http, time.localDateTime())
  val fuf = new FileUploadFrontendConnector(config, http)

  val fileBytes = Files.readAllBytes(Paths.get("README.md"))
  val fileBody = ByteString.fromArray(fileBytes)
  implicit val hc = HeaderCarrier()

  val result = for {
  // format: OFF
    envelopeId <- fu.createEnvelope(FormTypeId("testFormTypeId"))
    _          <- fuf.upload(envelopeId, FileId("README.md"), fileBody, ContentType.`text/plain`)

    x = println(s"envelope created: $envelopeId")
    _ = println(s"file uploaded: $envelopeId")
    // format: ON
  } yield ()

  Await.result(result, Duration.Inf)
}
