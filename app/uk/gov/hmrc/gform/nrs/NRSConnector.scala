/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.nrs

import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.{ Format, JsError, JsObject, JsString, JsSuccess, JsValue, Json, Reads, Writes }
import play.api.mvc.Request
import pureconfig.ConfigSource
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.auth.core.{ AuthConnector, AuthProvider, AuthProviders, MissingBearerToken, PlayAuthConnector }
import uk.gov.hmrc.gform.config.{ ConfigModule, HipConnectorConfig, NRSConnectorConfig }
import uk.gov.hmrc.gform.envelope.{ EnvelopeModule, EnvelopeService }
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.{ DestinationResult, NRSOrchestratorDestinationResult, UserSession }
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.{ NRSOrchestrator, nrsOrchestrator }
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, SessionKeys, StringContextOps }
import uk.gov.hmrc.objectstore.client.Path
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import scala.concurrent.{ ExecutionContext, Future }

private case class SubmissionRequestMetaData(
  businessId: String,
  notableEvent: String,
  payloadContentType: String,
  payloadSha256Checksum: String,
  attachmentIds: Option[Seq[String]],
  userSubmissionTimestamp: String,
  identityData: JsObject,
  userAuthToken: String,
  headerData: JsObject,
  searchKeys: JsObject
)

private object SubmissionRequestMetaData {
  implicit val format: Format[SubmissionRequestMetaData] = Json.format
}

private case class SubmissionRequest(payload: String, metadata: SubmissionRequestMetaData)

private object SubmissionRequest {
  implicit val format: Format[SubmissionRequest] = Json.format
}

case class NRSSubmissionResponse(nrSubmissionId: String)

object NRSSubmissionResponse {
  implicit val format: Format[NRSSubmissionResponse] = Json.format
}

private case class AttachmentRequest(
  attachmentUrl: String,
  attachmentId: String,
  attachmentSha256Checksum: String,
  attachmentContentType: String,
  nrSubmissionId: String
)

private object AttachmentRequest {
  implicit val format: Format[AttachmentRequest] = Json.format
}

case class NRSAttachment(url: String, id: String, sha256Checksum: String, contentType: String)

object NRSConnector {

  def generateSha256Checksum(payload: String): String =
    generateSha256Checksum(payload.getBytes(StandardCharsets.UTF_8))

  def generateSha256Checksum(payload: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(payload)
    new String(Hex.encode(hash))
  }
}

class NRSConnector(
  configModule: ConfigModule,
  httpClient: HttpClientV2,
  objectStoreModule: ObjectStoreModule,
  envelopeModule: EnvelopeModule
)(implicit ec: ExecutionContext) {

  private val envelopeService = envelopeModule.envelopeService
  private val authConnector: AuthConnector = new PlayAuthConnector {
    override val serviceUrl: String = configModule.serviceConfig.baseUrl("auth")
    override def httpClientV2: HttpClientV2 = httpClient
  }

  private val apiKey = configModule.nrsConfig.authorizationToken
  private val contentType = "application/json"

  private def makeCall[T](url: URL, body: T)(implicit writes: Writes[T], hc: HeaderCarrier) = {

    println(url)
    println(Json.prettyPrint(Json.toJson(body)))
    val headers = Seq(
      "Content-Type" -> contentType,
      "X-API-Key"    -> apiKey
    )
    httpClient
      .post(url)
      .setHeader(headers: _*)
      .withBody(Json.toJson(body))
      .execute[HttpResponse]
  }

  //Documentation: https://confluence.tools.tax.service.gov.uk/pages/viewpage.action?spaceKey=NR&title=Submission+API+specification
  private def createSubmission(
    destination: NRSOrchestrator,
    attachmentIds: Option[Seq[String]],
    payload: String,
    userSession: UserSession,
    nrsOrchestratorDestinationResult: NRSOrchestratorDestinationResult
  )(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val now = java.time.Instant.now().toString
    val attachmentCount = attachmentIds.toSeq.flatten.length
    if (attachmentCount > 30) {
      throw new RuntimeException(
        s"nrs-orchestrator has a limit of 30 files per submission. Attachment count: $attachmentCount"
      )
    }

    val userAuthToken = hc.authorization.getOrElse(throw MissingBearerToken("missing authorisation token")).value

    val payloadSha256Checksum: String = NRSConnector.generateSha256Checksum(payload)
    val headerData: JsObject = new JsObject(userSession.onSubmitHeaders.map(x => x._1 -> JsString(x._2)).toMap)

    def toBase64(str: String) = new String(Base64.getEncoder.encode(str.getBytes("UTF-8")), "UTF-8")

    val url = url"${configModule.serviceConfig.baseUrl("nrs-orchestrator")}/nrs-orchestrator/submission"

    val searchKeys = JsObject(
      Seq(
        nrsOrchestratorDestinationResult.data.saUtr.map("saUtr"                                 -> JsString(_)),
        nrsOrchestratorDestinationResult.data.ctUtr.map("ctUtr"                                 -> JsString(_)),
        nrsOrchestratorDestinationResult.data.submissionReferenceId.map("submissionReferenceId" -> JsString(_))
      ).flatten
    )

    for {
      identityData <- getRetrievals()
      body = SubmissionRequest(
               payload = toBase64(payload),
               SubmissionRequestMetaData(
                 businessId = destination.businessId,
                 notableEvent = destination.notableEvent,
                 payloadContentType = contentType,
                 payloadSha256Checksum = payloadSha256Checksum,
                 attachmentIds = attachmentIds,
                 userSubmissionTimestamp = now,
                 identityData = identityData,
                 userAuthToken = userAuthToken,
                 headerData = headerData,
                 searchKeys = searchKeys
               )
             )
      response <- makeCall(url, body)
    } yield response
  }

  private def getRetrievals()(implicit hc: HeaderCarrier) = {
    val retrieval = new Retrieval[JsObject] {
      override def propertyNames: Seq[String] = Seq(
        "internalId",
        "externalId",
        "agentCode",
        "credentials",
        "confidenceLevel",
        "nino",
        "saUtr",
        "name",
        "dateOfBirth",
        "email",
        "agentInformation",
        "groupIdentifier",
        "credentialRole",
        "mdtpInformation",
        "itmpName",
        "itmpDateOfBirth",
        "itmpAddress",
        "affinityGroup",
        "credentialStrength",
        "loginTimes"
      )

      override def reads: Reads[JsObject] = (json: JsValue) => json.validate[JsObject]
    }
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    authConnector.authorise(predicate, retrieval)
  }

  //Documentation: https://confluence.tools.tax.service.gov.uk/display/NR/Attachments+API+specification
  private def addObjectStoreAttachment(nrSubmissionId: String, attachment: NRSAttachment)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val url = url"${configModule.serviceConfig.baseUrl("nrs-orchestrator")}/nrs-orchestrator/attachment"
    val body = AttachmentRequest(
      attachmentUrl = attachment.url,
      attachmentId = attachment.id,
      attachmentSha256Checksum = attachment.sha256Checksum,
      attachmentContentType = attachment.contentType,
      nrSubmissionId = nrSubmissionId
    )
    makeCall(url, body)
  }

  private def retrieveAttachments(envelopeData: EnvelopeData, envelopeId: EnvelopeId)(implicit
    hc: HeaderCarrier
  ): Future[List[NRSAttachment]] =
    Future.sequence(envelopeData.files.filterNot(_.contentType == ContentType("application/pdf")).map { envelopeFile =>
      println(envelopeFile)
      val sha256: String = envelopeFile.metadata
        .getOrElse("sha256Checksum", throw new RuntimeException("No checksum available in meta data"))
        .head

      val path: Path.File = objectStoreModule.objectStoreConnector
        .directory(envelopeId.value, envelopeFile.subDirectory)
        .file(envelopeFile.fileName)
      val downloadUrlFtr = objectStoreModule.foptObjectStoreService.presignedDownloadUrl(path)
      downloadUrlFtr
        .map { downloadUrl =>
          NRSAttachment(
            downloadUrl.downloadUrl.toString,
            envelopeFile.fileId,
            sha256,
            envelopeFile.contentType.toString
          )
        }
        .value
        .map {
          case Right(nrsAttachment: NRSAttachment) => nrsAttachment
          case Left(_)                             => throw new RuntimeException("Unknown type expected NRSAttachment")
        }
    })

  case class NrsOrchestratorHttpResponse(
    submissionResponse: HttpResponse,
    attachmentResponses: List[HttpResponse]
  )

  def submit(
    envelopeId: EnvelopeId,
    destination: NRSOrchestrator,
    payload: String,
    userSession: UserSession,
    nrsOrchestratorDestinationResult: NRSOrchestratorDestinationResult
  )(implicit hc: HeaderCarrier): Future[NrsOrchestratorHttpResponse] = {
//    val nrsDestinationResults = NRSOrchestratorDestinationResult
//      .fromDestinationResult(destinationResult) match {
//      case JsSuccess(value, path) => value
//      case JsError(errors) => throw new RuntimeException(errors.toString())
//    }
    val envelopeDataFtr = envelopeService.get(envelopeId)

    val attachmentsFtr = envelopeDataFtr.flatMap(envelopeData => retrieveAttachments(envelopeData, envelopeId))
    val submissionFtr = attachmentsFtr.flatMap { attachments =>
      val attachmentIds = attachments.map(_.id)
      createSubmission(
        destination,
        if (attachmentIds.nonEmpty) Some(attachmentIds) else None,
        payload,
        userSession,
        nrsOrchestratorDestinationResult
      )
    }

    val attachmentResponsesFtr =
      (for {
        attachments        <- attachmentsFtr
        submissionResponse <- submissionFtr
      } yield
        if (submissionResponse.status != 200) {
          Future.successful(List())
        } else {
          val submissionId = Json.parse(submissionResponse.body).as[NRSSubmissionResponse].nrSubmissionId
          Future.sequence(
            attachments.map { attachment =>
              addObjectStoreAttachment(submissionId, attachment)
            }
          )
        }).flatten

    for {
      submissionResponse  <- submissionFtr
      attachmentResponses <- attachmentResponsesFtr
    } yield NrsOrchestratorHttpResponse(submissionResponse, attachmentResponses)
  }
}
