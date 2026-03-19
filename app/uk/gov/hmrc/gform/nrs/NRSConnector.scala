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

import cats.implicits.catsSyntaxEq
import org.bouncycastle.util.encoders.Hex
import org.json4s.native.JsonMethods
import org.json4s.native.Printer.compact
import org.slf4j.LoggerFactory
import play.api.libs.json.{ Format, JsObject, JsString, JsValue, Json, Reads, Writes }
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.auth.core.{ AuthConnector, AuthProvider, AuthProviders, MissingBearerToken, PlayAuthConnector }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, NRSOrchestratorDestinationResult, SubmissionRef, UserSession }
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.NRSOrchestrator
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.{ RoboticsXMLGenerator, Submission }
import uk.gov.hmrc.gform.submission.destinations.DestinationSubmissionInfo
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.objectstore.client.Path
import uk.gov.hmrc.http.HttpReads.Implicits._

import java.net.URL
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.{ Base64, UUID }
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import scala.util.matching.Regex

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
  nrSubmissionId: String,
  businessId: String,
  notableEvent: String
)

private object AttachmentRequest {
  implicit val format: Format[AttachmentRequest] = Json.format
}

case class NRSAttachment(url: String, id: String, sha256Checksum: String, contentType: String)

case class NrsOrchestratorHttpResponse(
  submissionResponse: HttpResponse,
  attachmentResponses: List[HttpResponse]
)

object NRSConnector {

  def generateSha256Checksum(payload: String): String =
    generateSha256Checksum(payload.getBytes(StandardCharsets.UTF_8))

  def generateSha256Checksum(payload: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest(payload)
    new String(Hex.encode(hash))
  }
}

case class NrsPayloadMetaData(`submission-reference`: String, correlationId: String, userLanguage: String)
object NrsPayloadMetaData {
  implicit val format: Format[NrsPayloadMetaData] = Json.format
}

case class NrsPayload(gform: JsObject, metaData: NrsPayloadMetaData)
object NrsPayload {
  implicit val format: Format[NrsPayload] = Json.format
}

class NRSConnector(
  configModule: ConfigModule,
  httpClient: HttpClientV2,
  objectStoreModule: ObjectStoreModule,
  envelopeModule: EnvelopeModule
)(implicit ec: ExecutionContext) {
  private lazy val envelopeService = envelopeModule.envelopeService
  private lazy val authConnector: AuthConnector = new PlayAuthConnector {
    override val serviceUrl: String = configModule.serviceConfig.baseUrl("auth")
    override def httpClientV2: HttpClientV2 = httpClient
  }

  private lazy val apiKey = configModule.nrsConfig.authorizationToken
  private lazy val contentType = "application/json"

  private lazy val logger = LoggerFactory.getLogger(getClass)

  private def makeCall[T](url: URL, body: T)(implicit writes: Writes[T], hc: HeaderCarrier) = {

    if (!configModule.isProd) {
      logger.warn("request: POST " + url)
      logger.warn(Json.prettyPrint(Json.toJson(body)))
    }
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
  //Local testing: https://github.com/hmrc/nrs-orchestrator/blob/main/LOCAL-CONFIG.md
  //Curl to add local auth access to nrs-orchestrator
  //curl -i -X POST -H 'Content-Type: application/json' -d '{ "token": "1234", "principal": "nrs-orchestrator", "permissions": [{ "resourceType": "object-store", "resourceLocation": "nrs-orchestrator", "actions": ["*"] }]}' 'http://localhost:8470/test-only/token'
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

    val searchKeys = Json.toJson(nrsOrchestratorDestinationResult.data.searchKeys).as[JsObject]

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
  private def addObjectStoreAttachment(
    nrSubmissionId: String,
    attachment: NRSAttachment,
    nrsOrchestrator: NRSOrchestrator
  )(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    val url = url"${configModule.serviceConfig.baseUrl("nrs-orchestrator")}/nrs-orchestrator/attachment"
    val body = AttachmentRequest(
      attachmentUrl = attachment.url,
      attachmentId = attachment.id,
      attachmentSha256Checksum = attachment.sha256Checksum,
      attachmentContentType = attachment.contentType,
      nrSubmissionId = nrSubmissionId,
      businessId = nrsOrchestrator.businessId,
      notableEvent = nrsOrchestrator.notableEvent
    )
    makeCall(url, body)
  }

  private def retrieveAttachments(envelopeData: EnvelopeData, envelopeId: EnvelopeId, submissionRef: SubmissionRef)(
    implicit hc: HeaderCarrier
  ): Future[List[NRSAttachment]] =
    Future.sequence(
      envelopeData.files.filterNot(file => file.fileName.startsWith(submissionRef.withoutHyphens)).map { envelopeFile =>
        val sha256: String = envelopeFile.metadata
          .getOrElse("sha256Checksum", throw new RuntimeException("No checksum available in meta data"))
          .head

        val path: Path.File = objectStoreModule.objectStoreConnector
          .directory(envelopeId.value, envelopeFile.subDirectory)
          .file(envelopeFile.fileName)
        val fileId = UUID.randomUUID().toString
        val downloadUrlFtr = objectStoreModule.foptObjectStoreService.presignedDownloadUrl(path)
        downloadUrlFtr
          .map { downloadUrl =>
            NRSAttachment(
              downloadUrl.downloadUrl.toString,
              fileId,
              sha256,
              envelopeFile.contentType.value
            )
          }
          .value
          .map {
            case Right(nrsAttachment: NRSAttachment) => nrsAttachment
            case Left(_)                             => throw new RuntimeException("Unknown type expected NRSAttachment")
          }
      }
    )

  private def createPayload(
    structuredFormData: StructuredFormValue.ObjectStructure,
    submission: Submission,
    l: LangADT
  ): NrsPayload = {

    def replacePII(value: String): String = {
      val stringRegex: Regex = """"[^:"]*":\s*".*"""".r

      stringRegex.replaceAllIn(
        value,
        regexMatch => {
          val matchedString: String = regexMatch.matched
          val (field, value) = matchedString.splitAt(matchedString.indexOf(":"))
          val numberOfSpaces = value.splitAt(value.indexOf(":") + 1)._2.takeWhile(_ === ' ').length
          val valueLength: Int = value.length - 3 - numberOfSpaces // Minus 3 for the colon and quotation marks
          field + s""":${" " * numberOfSpaces}"$valueLength""""
        }
      )
    }

    def convertToJson(payload: String): JsObject =
      Try {
        Json.parse(payload)
      }.fold(
        error => {
          val errorMessageWithoutPii = replacePII(error.getMessage)
          val fullJsonWithoutPii: String = replacePII(payload)

          throw new Exception(
            s"nrsOrchestrator destination error, failed to parse payload into a json:\n$errorMessageWithoutPii, full json:\n$fullJsonWithoutPii"
          )
        },
        jsValue =>
          jsValue.asOpt[JsObject].getOrElse {
            throw new Exception(
              s"nrsOrchestrator destination error. Cannot convert payload into a JsObject. This is not a JsObject: '$jsValue'"
            )
          }
      )

    val rawFormDataBasedPayload = compact(
      JsonMethods.render(
        org.json4s.Xml.toJson(
          RoboticsXMLGenerator.buildDataStoreXML(
            structuredFormData,
            sanitizeRequired = false
          )
        )
      )
    )

    val formDataBasedPayload: JsObject = convertToJson(rawFormDataBasedPayload)

    NrsPayload(
      formDataBasedPayload.value("gform").as[JsObject],
      NrsPayloadMetaData(
        submission.submissionRef.value,
        submission.envelopeId.value,
        l.langADTToString.toUpperCase
      )
    )
  }

  def submit(
    envelopeId: EnvelopeId,
    destination: NRSOrchestrator,
    userSession: UserSession,
    nrsOrchestratorDestinationResult: NRSOrchestratorDestinationResult,
    submission: DestinationSubmissionInfo,
    structuredFormValue: StructuredFormValue.ObjectStructure,
    l: LangADT
  )(implicit hc: HeaderCarrier): Future[NrsOrchestratorHttpResponse] = {
    val envelopeDataFtr = envelopeService.get(envelopeId)
    val payload = createPayload(structuredFormValue, submission.submission, l)

    val attachmentsFtr = envelopeDataFtr.flatMap(envelopeData =>
      retrieveAttachments(envelopeData, envelopeId, submission.submission.submissionRef)
    )
    val submissionFtr = attachmentsFtr.flatMap { attachments =>
      val attachmentIds = attachments.map(_.id)
      createSubmission(
        destination,
        if (attachmentIds.nonEmpty) Some(attachmentIds) else None,
        Json.stringify(Json.toJson(payload)),
        userSession,
        nrsOrchestratorDestinationResult
      )
    }

    val attachmentResponsesFtr =
      (for {
        attachments        <- attachmentsFtr
        submissionResponse <- submissionFtr
      } yield
        if (submissionResponse.status != 202) {
          Future.successful(List())
        } else {
          val submissionId = Json.parse(submissionResponse.body).as[NRSSubmissionResponse].nrSubmissionId
          Future.sequence(
            attachments.map { attachment =>
              addObjectStoreAttachment(submissionId, attachment, destination)
            }
          )
        }).flatten

    for {
      submissionResponse  <- submissionFtr
      attachmentResponses <- attachmentResponsesFtr
    } yield NrsOrchestratorHttpResponse(submissionResponse, attachmentResponses)
  }
}
