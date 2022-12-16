/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.objectStore

import akka.util.ByteString
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.objectstore.{ ObjectStoreConnector, ObjectStoreService }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.{ Available, EnvelopeData, EnvelopeFile }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.Path.File
import uk.gov.hmrc.objectstore.client.{ Md5Hash, ObjectSummaryWithMd5 }

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ObjectStoreServiceSpec extends AnyFlatSpec with Matchers with ScalaFutures with MockFactory {
  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val fileName = "file-name"
  private val byteString = ByteString("byte-string")
  private val envelopeId = EnvelopeId("envelope-id")
  private val fileId = FileId("file-id")
  private val expectedObjectSummary = ObjectSummaryWithMd5(File("/test"), 1L, Md5Hash("md5"), Instant.now())
  private val expectedEnvelopeData = EnvelopeData(
    envelopeId,
    List.empty[EnvelopeFile]
  )
  private val expectedSavedEnvelopeData = expectedEnvelopeData.copy(files =
    expectedEnvelopeData.files :+ EnvelopeFile(
      fileId.value,
      fileName,
      Available,
      ContentType.`application/pdf`,
      1L,
      Map.empty
    )
  )

  "getFileBytes" should "get file as byte stream" in {

    fixture
      .expectGetFileBytes(envelopeId, fileName)
      .service
      .getFileBytes(envelopeId, fileName)
      .futureValue shouldBe byteString
  }

  "uploadFile" should "get file information , upload file and amend file information" in {

    fixture
      .expectGet(envelopeId)
      .expectUploadFile(envelopeId, fileName, byteString, Some(ContentType.`application/pdf`.value))
      .expectSave(expectedSavedEnvelopeData)
      .service
      .uploadFile(envelopeId, fileId, fileName, byteString, ContentType.`application/pdf`)
      .futureValue shouldBe expectedObjectSummary
  }

  "deleteFile" should "return Runtime exception when stored data does not exist any file" in {

    fixture
      .expectGetForDelete(envelopeId)
      .expectDeleteFile(envelopeId, fileName)
      .expectSave(expectedEnvelopeData)
      .service
      .deleteFile(envelopeId, fileId)
      .futureValue shouldBe Future.unit.futureValue
  }

  case class Fixture(
    service: ObjectStoreService,
    objectStoreConnector: ObjectStoreConnector,
    envelopeService: EnvelopeAlgebra[Future]
  ) {

    def expectGetFileBytes(envelopeId: EnvelopeId, fileName: String): Fixture = {
      (objectStoreConnector
        .getFileBytes(_: EnvelopeId, _: String)(
          _: HeaderCarrier
        ))
        .expects(
          envelopeId,
          fileName,
          hc
        )
        .returning(Future.successful(byteString))

      this
    }

    def expectGet(envelopeId: EnvelopeId): Fixture = {
      (envelopeService
        .get(_: EnvelopeId))
        .expects(envelopeId)
        .returning(Future.successful(expectedEnvelopeData))

      this
    }

    def expectUploadFile(
      envelopeId: EnvelopeId,
      fileName: String,
      content: ByteString,
      contentType: Option[String]
    ): Fixture = {
      (objectStoreConnector
        .uploadFile(_: EnvelopeId, _: String, _: ByteString, _: Option[String])(
          _: HeaderCarrier
        ))
        .expects(envelopeId, fileName, content, contentType, hc)
        .returning(Future.successful(expectedObjectSummary))

      this
    }

    def expectSave(envelope: EnvelopeData): Fixture = {
      (envelopeService
        .save(_: EnvelopeData))
        .expects(envelope)
        .returning(Future.unit)

      this
    }

    def expectDeleteFile(envelopeId: EnvelopeId, fileName: String): Fixture = {
      (objectStoreConnector
        .deleteFile(_: EnvelopeId, _: String)(
          _: HeaderCarrier
        ))
        .expects(envelopeId, fileName, hc)
        .returning(Future.unit)

      this
    }

    def expectGetForDelete(envelopeId: EnvelopeId): Fixture = {
      (envelopeService
        .get(_: EnvelopeId))
        .expects(envelopeId)
        .returning(Future.successful(expectedSavedEnvelopeData))

      this
    }
  }

  def fixture(): Fixture = {
    val objectStoreConnector = mock[ObjectStoreConnector]
    val envelopeAlgebra = mock[EnvelopeAlgebra[Future]]

    Fixture(
      new ObjectStoreService(objectStoreConnector, envelopeAlgebra),
      objectStoreConnector,
      envelopeAlgebra
    )
  }

}
