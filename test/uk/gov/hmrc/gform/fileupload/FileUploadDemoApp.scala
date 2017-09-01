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

package uk.gov.hmrc.gform.fileupload

import java.nio.file.{ Files, Paths }

import akka.util.ByteString
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.gform.wshttp.TestWSHttp
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object FileUploadDemoApp extends App {

  val configLocal = FUConfig(
    fileUploadBaseUrl = "http://localhost:8898",
    fileUploadFrontendBaseUrl = "http://localhost:8899",
    expiryDays = 30,
    maxSize = "20MB",
    maxSizePerItem = "10MB",
    maxItems = 3,
    contentTypes = List(
      ContentType.`application/pdf`,
      ContentType.`application/xml`,
      ContentType.`image/jpeg`,
      ContentType.`text/xml`,
      ContentType.`application/vnd.ms-excel`,
      ContentType.`application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`
    )
  )

  val configLocalProxiedByGform = configLocal.copy(
    //TIP run GFORM using `runInLocal.sh` script
    fileUploadBaseUrl = "http://localhost:9195/submissions/test-only/proxy-to-file-upload"
  )

  val configDevProxiedByGform = configLocal.copy(
    fileUploadBaseUrl = "https://www-dev.tax.service.gov.uk/submissions/test-only/proxy-to-file-upload",
    fileUploadFrontendBaseUrl = "https://www-dev.tax.service.gov.uk"
  )

  val config = configLocal

  val http = TestWSHttp

  val timeProvider = new TimeProvider {}
  val fu = new FileUploadConnector(config, http, timeProvider)
  val fuf = new FileUploadFrontendConnector(config, http)
  val fileUploadService = new FileUploadService(fu, fuf)

  val fileBytes = Files.readAllBytes(Paths.get("README.md"))
  val fileBody = ByteString.fromArray(fileBytes)
  implicit val hc = HeaderCarrier()

  val result = for {
  // format: OFF
    envelopeId <- fileUploadService.createEnvelope(FormTemplateId("testFormTypeId"))
    _          <- fuf.upload(envelopeId, FileId("README.md"), "README.md", fileBody, ContentType.`text/xml`)
    envelope   <- fu.getEnvelope(envelopeId)
    _          <- fileUploadService.deleteFile(envelopeId, FileId("README.md"))
    envelopeNoFile   <- fu.getEnvelope(envelopeId)
    _ = println(envelope)
    _ = println(envelopeNoFile)
    // format: ON
  } yield ()

  Await.result(result, Duration.Inf)
  http.stop().onComplete(_ =>
    //I don't know how to gracefully stop it
    System.exit(0))

}
