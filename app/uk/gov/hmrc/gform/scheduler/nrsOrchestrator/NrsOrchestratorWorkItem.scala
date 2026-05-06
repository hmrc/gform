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
import uk.gov.hmrc.gform.nrs.{ BusinessId, NrsPayload }
import uk.gov.hmrc.gform.save4later.EncryptedFormat
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ NRSOrchestratorDestinationResultData, SubmissionRef }

final case class NrsOrchestratorWorkItemOld(
  envelopeId: EnvelopeId,
  businessId: BusinessId,
  notableEvent: String,
  onSubmitHeaders: Seq[(String, String)],
  destinationResultData: NRSOrchestratorDestinationResultData,
  submissionRef: SubmissionRef,
  payload: NrsPayload,
  submissionDate: String,
  userAuthToken: String,
  identityData: JsObject
)

object NrsOrchestratorWorkItemOld {
  private val format: OFormat[NrsOrchestratorWorkItemOld] = Json.format[NrsOrchestratorWorkItemOld]
  def readsEncrypted(implicit jsonCrypto: Encrypter with Decrypter): Reads[NrsOrchestratorWorkItemOld] =
    EncryptedFormat.formatEncrypted(jsonCrypto)(format)
}

final case class NrsOrchestratorWorkItemData(
  businessId: BusinessId,
  notableEvent: String,
  onSubmitHeaders: Seq[(String, String)],
  destinationResultData: NRSOrchestratorDestinationResultData,
  payload: NrsPayload,
  submissionDate: String,
  userAuthToken: String,
  identityData: JsObject
)

final case class NrsOrchestratorWorkItem(
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  data: NrsOrchestratorWorkItemData
)

object NrsOrchestratorWorkItem {
  private val logger = LoggerFactory.getLogger(getClass)
  implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  def formatEncrypted(implicit jsonCrypto: Encrypter with Decrypter): Format[NrsOrchestratorWorkItem] = {
    implicit val encryptedDataFormat: Format[NrsOrchestratorWorkItemData] =
      EncryptedFormat.formatEncrypted(jsonCrypto)(Json.format[NrsOrchestratorWorkItemData])
    new Format[NrsOrchestratorWorkItem] {
      override def reads(json: JsValue): JsResult[NrsOrchestratorWorkItem] =
        Json.reads[NrsOrchestratorWorkItem].reads(json) match {
          case JsError(errors) =>
            logger.warn(s"NrsOrchestratorWorkItem old json serialization fallback. errors: $errors")
            NrsOrchestratorWorkItemOld.readsEncrypted.reads(json).map { workItem =>
              NrsOrchestratorWorkItem(
                workItem.envelopeId,
                FormTemplateId(""),
                workItem.submissionRef,
                NrsOrchestratorWorkItemData(
                  workItem.businessId,
                  workItem.notableEvent,
                  workItem.onSubmitHeaders,
                  workItem.destinationResultData,
                  workItem.payload,
                  workItem.submissionDate,
                  workItem.userAuthToken,
                  workItem.identityData
                )
              )
            } //TODO: This is a fallback for old workItem structure, remove this and NrsOrchestratorWorkItemOld when database is cleaned-up.
          case success: JsSuccess[NrsOrchestratorWorkItem] => success
        }
      override def writes(o: NrsOrchestratorWorkItem): JsValue = Json.writes[NrsOrchestratorWorkItem].writes(o)
    }
  }
}
