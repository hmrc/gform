/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.nrs.{ BusinessId, NRSAttachment }
import uk.gov.hmrc.gform.save4later.EncryptedFormat
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

final case class NrsOrchestratorAttachmentWorkItemOld(
  nrSubmissionId: String,
  attachment: NRSAttachment,
  businessId: BusinessId,
  notableEvent: String
)

object NrsOrchestratorAttachmentWorkItemOld {
  private val format: OFormat[NrsOrchestratorAttachmentWorkItemOld] = Json.format[NrsOrchestratorAttachmentWorkItemOld]
  def readsEncrypted(implicit jsonCrypto: Encrypter with Decrypter): Reads[NrsOrchestratorAttachmentWorkItemOld] =
    EncryptedFormat.formatEncrypted(jsonCrypto)(format)
}

final case class NrsOrchestratorAttachmentWorkItemData(
  nrSubmissionId: String,
  attachment: NRSAttachment,
  businessId: BusinessId,
  notableEvent: String
)

final case class NrsOrchestratorAttachmentWorkItem(
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  data: NrsOrchestratorAttachmentWorkItemData
)

object NrsOrchestratorAttachmentWorkItem {
  private val logger = LoggerFactory.getLogger(getClass)
  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  def formatEncrypted(implicit jsonCrypto: Encrypter with Decrypter): Format[NrsOrchestratorAttachmentWorkItem] = {
    implicit val encryptedDataFormat: Format[NrsOrchestratorAttachmentWorkItemData] =
      EncryptedFormat.formatEncrypted(jsonCrypto)(Json.format[NrsOrchestratorAttachmentWorkItemData])
    new Format[NrsOrchestratorAttachmentWorkItem] {
      override def reads(json: JsValue): JsResult[NrsOrchestratorAttachmentWorkItem] =
        Json.reads[NrsOrchestratorAttachmentWorkItem].reads(json) match {
          case JsError(errors) =>
            logger.warn(s"NrsOrchestratorAttachmentWorkItem old json serialization fallback. errors: $errors")
            NrsOrchestratorAttachmentWorkItemOld.readsEncrypted.reads(json).map { workItem =>
              NrsOrchestratorAttachmentWorkItem(
                EnvelopeId(""),
                FormTemplateId(""),
                SubmissionRef(""),
                NrsOrchestratorAttachmentWorkItemData(
                  workItem.nrSubmissionId,
                  workItem.attachment,
                  workItem.businessId,
                  workItem.notableEvent
                )
              )
            } //TODO: This is a fallback for old workItem structure, remove this and NrsOrchestratorAttachmentWorkItemOld when database is cleaned-up.
          case success: JsSuccess[NrsOrchestratorAttachmentWorkItem] => success
        }
      override def writes(o: NrsOrchestratorAttachmentWorkItem): JsValue =
        Json.writes[NrsOrchestratorAttachmentWorkItem].writes(o)
    }
  }
}
