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

package uk.gov.hmrc.bforms.services

import cats.data.EitherT
import cats.implicits._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{ JsObject, Json }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.bforms.exceptions.{ InvalidState, UnexpectedState }
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.model.{ EnvelopeId, FileId, MetadataXml, RouteEnvelopeRequest, UploadFile }
import uk.gov.hmrc.bforms.typeclasses.Now
import uk.gov.hmrc.play.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.bforms.model._
import uk.gov.hmrc.bforms.typeclasses.{ FusFeUrl, FusUrl, Post, ServiceUrl }

object FileUploadService {

  def createEnvelope(
    formTypeId: FormTypeId
  )(
    implicit
    ec: ExecutionContext,
    createEnvelope: Post[CreateEnvelope, HttpResponse]
  ): Future[Opt[EnvelopeId]] =
    createEnvelope(envelopeRequest(formTypeId)).map(extractEnvelopId)

  def submitEnvelope(
    submissionAndPdf: SubmissionAndPdf
  )(
    implicit
    ec: ExecutionContext,
    uploadFile: Post[UploadFile, HttpResponse],
    routeEnvelope: Post[RouteEnvelopeRequest, HttpResponse],
    now: Now[LocalDateTime] // this will make sure that the same instance of now is used throughout body if this method
  ): ServiceResponse[String] = {

    val submissionRef: SubmissionRef = submissionAndPdf.submission.submissionRef

    val envelopeId = submissionAndPdf.submission.envelopeId

    val date = now().format(DateTimeFormatter.ofPattern("YYYYMMdd"))
    val fileNamePrefix = s"$submissionRef-$date"

    val reconciliationId = ReconciliationId.create(submissionRef)
    val metadataXml = MetadataXml.xmlDec + "\n" + MetadataXml.getXml(submissionRef, reconciliationId, submissionAndPdf)

    // format: OFF
    for {
      _ <- fromFutureA(uploadFile(UploadFile(envelopeId, FileId("pdf"), s"$fileNamePrefix-iform.pdf", "application/pdf", submissionAndPdf.pdfSummary.pdfContent)))
      _ <- fromFutureA(uploadFile(UploadFile(envelopeId, FileId("xmlDocument"), s"$fileNamePrefix-metadata.xml", "application/xml; charset=UTF-8", metadataXml.getBytes)))
      _ <- fromFutureA(routeEnvelope(RouteEnvelopeRequest(envelopeId, "dfs", "DMS")))
    } yield {
      s"http://localhost:8898/file-transfer/envelopes/$envelopeId"
    }
    // format: ON
  }

  def extractEnvelopId(
    resp: HttpResponse
  ): Opt[EnvelopeId] = {
    resp.header(LOCATION) match {
      case Some(location) => location match {
        case EnvelopeIdExtractor(envelopeId) => Right(EnvelopeId(envelopeId))
        case otherwise => Left(InvalidState(s"EnvelopeId in $LOCATION header: $location not found"))
      }
      case None => Left(InvalidState(s"Header $LOCATION not found"))
    }
  }

  val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'HH:mm:ss'Z'")

  def envelopeRequest(
    formTypeId: FormTypeId
  )(
    implicit
    now: Now[LocalDateTime]
  ): CreateEnvelope = {

    def envelopeExpiryDate(numberOfDays: Int) = now().plusDays(numberOfDays).format(formatter)

    val json = Json.obj(
      "constraints" -> Json.obj(
        "contentTypes" -> Json.arr(
          "application/pdf",
          "image/jpeg"
        ),
        "maxItems" -> 5,
        "masSize" -> "30MB",
        "maxSizePerItem" -> "5MB"
      ),
      "callbackUrl" -> "someCallback",
      "expiryDate" -> s"${envelopeExpiryDate(7)}",
      "metadata" -> Json.obj(
        "application" -> "Digital Forms Service",
        "formTypeId" -> s"$formTypeId"
      )
    )

    CreateEnvelope(json)
  }
}
