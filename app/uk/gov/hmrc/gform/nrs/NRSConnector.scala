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
import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.Retrieval
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.scheduler.nrsOrchestrator.{ NrsOrchestratorAttachmentWorkItem, NrsOrchestratorAttachmentWorkItemData, NrsOrchestratorAttachmentWorkItemRepo, NrsOrchestratorWorkItem, NrsOrchestratorWorkItemData, NrsOrchestratorWorkItemRepo }
import uk.gov.hmrc.gform.sharedmodel.envelope.EnvelopeData
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, NRSOrchestratorDestinationResultData, SubmissionRef }
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.mongo.workitem.{ ProcessingStatus, WorkItem }
import uk.gov.hmrc.objectstore.client.{ Path, PresignedDownloadUrl }

import java.net.URL
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.{ Base64, UUID }
import scala.concurrent.{ ExecutionContext, Future }

private case class SubmissionRequestMetaData(
  businessId: BusinessId,
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
  businessId: BusinessId,
  notableEvent: String
)

private object AttachmentRequest {
  implicit val format: Format[AttachmentRequest] = Json.format
}

case class NRSAttachment(
  id: String,
  sha256Checksum: String,
  contentType: String,
  fileName: String,
  envelopeId: EnvelopeId,
  subDirectory: Option[String]
) {
  def getPresignedUrl(objectStoreModule: ObjectStoreModule)(implicit hc: HeaderCarrier): FOpt[PresignedDownloadUrl] = {
    val path: Path.File = objectStoreModule.objectStoreConnector
      .directory(envelopeId.value, subDirectory)
      .file(fileName)
    objectStoreModule.foptObjectStoreService.presignedDownloadUrl(path)
  }
}

object NRSAttachment {
  implicit val format: Format[NRSAttachment] = Json.format
}

object NRSConnector {

  val requiredSearchKeys: Map[BusinessId, Set[String]] = Map(
    BusinessId("vap") -> Set("taxpayerIdReference", "taxpayerId", "submissionReferenceId")
  )

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

case class NrsPayload(gform: FormData, metaData: NrsPayloadMetaData)
object NrsPayload {
  implicit val format: Format[NrsPayload] = Json.format
}

class NRSConnector(
  baseUrl: String,
  submissionUrl: String,
  attachmentUrl: String,
  httpClient: HttpClientV2,
  objectStoreModule: ObjectStoreModule,
  envelopeService: EnvelopeAlgebra[Future],
  authConnector: AuthConnector,
  apiKeys: Map[BusinessId, String],
  nrsOrchestratorWorkItemRepo: NrsOrchestratorWorkItemRepo,
  nrsOrchestratorAttachmentWorkItemRepo: NrsOrchestratorAttachmentWorkItemRepo
)(implicit ec: ExecutionContext) {

  private val contentType = "application/json"

  private val logger = LoggerFactory.getLogger(getClass)

  private def makeCall[T](url: URL, body: T, apiKey: String)(implicit writes: Writes[T], hc: HeaderCarrier) = {
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

  /*
    Documentation: https://confluence.tools.tax.service.gov.uk/pages/viewpage.action?spaceKey=NR&title=Submission+API+specification
    Local testing: https://github.com/hmrc/nrs-orchestrator/blob/main/LOCAL-CONFIG.md
    NRS stubs to test work-item: https://github.com/hmrc/nrs-stubs
    Curl to add local auth access to nrs-orchestrator
    curl -i -X POST -H 'Content-Type: application/json' -d '{ "token": "1234", "principal": "nrs-orchestrator", "permissions": [{ "resourceType": "object-store", "resourceLocation": "nrs-orchestrator", "actions": ["*"] }]}' 'http://localhost:8470/test-only/token'

    Example local nrs orchestrator:
    sm2 --start NRS_ORCHESTRATOR -appendArgs '{
    "NRS_ORCHESTRATOR": [
      "-J-Dnonrep-submission.clients.vap.apiKeySha256=334ad56b6df663c2909f6563f6955d070b68498d2307338f7de3e22303344ca7",
      "-J-Dnonrep-submission.clients.vap.notableEvents.abc.1=taxpayerIdReference","-J-Dnonrep-submission.clients.vap.attachmentsAllowed=true"
    ] }'
   */
  private def createSubmission(
    businessId: BusinessId,
    notableEvent: String,
    attachmentIds: Option[Seq[String]],
    payload: NrsPayload,
    onSubmitHeaders: Seq[(String, String)],
    destinationResultData: NRSOrchestratorDestinationResultData,
    submissionDate: String,
    userAuthToken: String,
    identityData: JsObject
  )(implicit hc: HeaderCarrier): Future[HttpResponse] = {

    val attachmentCount = attachmentIds.toSeq.flatten.length
    if (attachmentCount > 30) {
      throw new RuntimeException(
        s"nrs-orchestrator has a limit of 30 files per submission. Attachment count: $attachmentCount"
      )
    }

    val payloadJson = Json.stringify(Json.toJson(payload))
    val payloadSha256Checksum: String = NRSConnector.generateSha256Checksum(payloadJson)
    val headerData: JsObject = new JsObject(onSubmitHeaders.map(x => x._1 -> JsString(x._2)).toMap)

    def toBase64(str: String) = new String(Base64.getEncoder.encode(str.getBytes("UTF-8")), "UTF-8")
    val urlStr = s"$baseUrl/$submissionUrl"
    val url = url"$urlStr"

    val searchKeys = Json.toJson(destinationResultData.searchKeys).as[JsObject]

    val body = SubmissionRequest(
      payload = toBase64(payloadJson),
      SubmissionRequestMetaData(
        businessId = businessId,
        notableEvent = notableEvent,
        payloadContentType = contentType,
        payloadSha256Checksum = payloadSha256Checksum,
        attachmentIds = attachmentIds,
        userSubmissionTimestamp = submissionDate,
        identityData = identityData,
        userAuthToken = userAuthToken,
        headerData = headerData,
        searchKeys = searchKeys
      )
    )
    val apiKey = getApiKey(businessId)
    makeCall(url, body, apiKey)
  }

  def getRetrievals()(implicit hc: HeaderCarrier): Future[JsObject] = {
    val retrieval = new Retrieval[JsObject] {
      override def propertyNames: Seq[String] =
        (Retrievals.internalId and
          Retrievals.externalId and
          Retrievals.agentCode and
          Retrievals.credentials and
          Retrievals.confidenceLevel and
          Retrievals.nino and
          Retrievals.saUtr and
          Retrievals.dateOfBirth and
          Retrievals.email and
          Retrievals.agentInformation and
          Retrievals.groupIdentifier and
          Retrievals.credentialRole and
          Retrievals.mdtpInformation and
          Retrievals.itmpName and
          Retrievals.itmpDateOfBirth and
          Retrievals.itmpAddress and
          Retrievals.affinityGroup and
          Retrievals.credentialStrength and
          Retrievals.loginTimes).propertyNames

      override def reads: Reads[JsObject] = (json: JsValue) => json.validate[JsObject]
    }
    val predicate = AuthProviders(AuthProvider.GovernmentGateway)
    authConnector.authorise(predicate, retrieval)
  }

  private def issueAttachmentWorkItem(
    nrSubmissionId: String,
    attachment: NRSAttachment,
    businessId: BusinessId,
    notableEvent: String,
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef
  ) = {

    val workItem = nrsOrchestratorAttachmentWorkItemRepo.pushNew(
      NrsOrchestratorAttachmentWorkItem(
        envelopeId,
        formTemplateId,
        submissionRef,
        NrsOrchestratorAttachmentWorkItemData(nrSubmissionId, attachment, businessId, notableEvent)
      )
    )
    workItem.foreach { workItem =>
      logger.info(s"""NRS Orchestrator destination attachment workItem was created.
                     |workItem Id: ${workItem.id.toHexString}
                     |envelopeId: $envelopeId
                     |""".stripMargin)
    }
    workItem
  }

  private def getApiKey(businessId: BusinessId): String =
    apiKeys.get(businessId).getOrElse(throw new Exception(s"NRS api key not found for $businessId"))

  //Documentation: https://confluence.tools.tax.service.gov.uk/display/NR/Attachments+API+specification
  def submitObjectStoreAttachment(
    nrSubmissionId: String,
    attachment: NRSAttachment,
    businessId: BusinessId,
    notableEvent: String
  )(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val presignedUrl = attachment.getPresignedUrl(objectStoreModule)
    val urlStr = s"$baseUrl/$attachmentUrl"
    val url = url"$urlStr"
    presignedUrl
      .map { presignedUrl =>
        val body = AttachmentRequest(
          attachmentUrl = presignedUrl.downloadUrl.toString,
          attachmentId = attachment.id,
          attachmentSha256Checksum = attachment.sha256Checksum,
          attachmentContentType = attachment.contentType,
          nrSubmissionId = nrSubmissionId,
          businessId = businessId,
          notableEvent = notableEvent
        )
        val apiKey = getApiKey(businessId)
        makeCall(url, body, apiKey)
      }
      .value
      .flatMap {
        case Left(er)     => throw new RuntimeException(s"Unexpected state: $er")
        case Right(value) => value
      }

  }

  private def retrieveAttachments(
    envelopeData: EnvelopeData,
    envelopeId: EnvelopeId,
    submissionRef: SubmissionRef
  ): List[NRSAttachment] =
    envelopeData.files
      .filterNot(file =>
        file.fileName.startsWith(submissionRef.withoutHyphens) || file.subDirectory.nonEmpty
      ) //Filter out gform generated files and sub directories
      .map { envelopeFile =>
        val sha256: String = envelopeFile.metadata
          .getOrElse("sha256Checksum", throw new RuntimeException("No checksum available in meta data"))
          .head

        val fileId = UUID.randomUUID().toString

        NRSAttachment(
          fileId,
          sha256,
          envelopeFile.contentType.value,
          envelopeFile.fileName,
          envelopeId,
          envelopeFile.subDirectory
        )
      }

  def createPayload(
    formData: FormData,
    submission: Submission,
    l: LangADT
  ): NrsPayload =
    NrsPayload(
      formData,
      NrsPayloadMetaData(
        submission.submissionRef.value,
        submission.envelopeId.value,
        l.langADTToString.toUpperCase
      )
    )

  def nrsServerFailure(httpResponse: HttpResponse): Boolean = {
    val status = httpResponse.status
    status == 422 || (status >= 500 && status < 600)
  }

  def issueSubmissionWorkItem(
    envelopeId: EnvelopeId,
    businessId: BusinessId,
    notableEvent: String,
    onSubmitHeaders: Seq[(String, String)],
    destinationResultData: NRSOrchestratorDestinationResultData,
    submissionRef: SubmissionRef,
    payload: NrsPayload,
    userAuthToken: String,
    identityData: JsObject,
    submissionDate: String,
    formTemplateId: FormTemplateId
  ): Future[WorkItem[NrsOrchestratorWorkItem]] =
    nrsOrchestratorWorkItemRepo
      .pushNew(
        NrsOrchestratorWorkItem(
          envelopeId,
          formTemplateId,
          submissionRef,
          NrsOrchestratorWorkItemData(
            businessId,
            notableEvent,
            onSubmitHeaders,
            destinationResultData,
            payload,
            submissionDate,
            userAuthToken,
            identityData
          )
        ),
        initialState = _ => ProcessingStatus.Deferred
      )

  def submit(
    envelopeId: EnvelopeId,
    businessId: BusinessId,
    notableEvent: String,
    onSubmitHeaders: Seq[(String, String)],
    destinationResultData: NRSOrchestratorDestinationResultData,
    submissionRef: SubmissionRef,
    payload: NrsPayload,
    userAuthToken: String,
    identityData: JsObject,
    submissionDate: String,
    formTemplateId: FormTemplateId
  )(implicit hc: HeaderCarrier): Future[HttpResponse] = {

    def getAttachments(envelope: EnvelopeData): List[NRSAttachment] =
      retrieveAttachments(envelope, envelopeId, submissionRef)

    def submissionFtr(attachments: List[NRSAttachment]): Future[HttpResponse] = {
      val attachmentIds = attachments.map(_.id)
      createSubmission(
        businessId,
        notableEvent,
        if (attachmentIds.nonEmpty) Some(attachmentIds) else None,
        payload,
        onSubmitHeaders,
        destinationResultData,
        submissionDate,
        userAuthToken,
        identityData
      )
    }

    def issueAttachmentWorkItems(
      submissionResponse: HttpResponse,
      attachments: List[NRSAttachment]
    ): Future[List[WorkItem[NrsOrchestratorAttachmentWorkItem]]] =
      if (submissionResponse.status == 202) {
        val submissionId = Json
          .parse(submissionResponse.body)
          .validate[NRSSubmissionResponse]
          .getOrElse(
            throw new RuntimeException(
              s"Invalid response from NRS submission. Status: 202. Body: ${submissionResponse.body}"
            )
          )
          .nrSubmissionId
        Future.sequence(
          attachments.map { attachment =>
            issueAttachmentWorkItem(
              submissionId,
              attachment,
              businessId,
              notableEvent,
              envelopeId,
              formTemplateId,
              submissionRef
            )
          }
        )
      } else {
        Future.successful(List())
      }

    for {
      envelope <- envelopeService.get(envelopeId)
      attachments = getAttachments(envelope)
      submissionResponse <- submissionFtr(attachments)
      _                  <- issueAttachmentWorkItems(submissionResponse, attachments)
    } yield submissionResponse
  }
}
