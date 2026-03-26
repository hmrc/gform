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

import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.nrs.NRSAttachment
import reflect.runtime.universe._

final case class NrsOrchestratorAttachmentWorkItem(
  nrSubmissionId: String,
  attachment: NRSAttachment,
  businessId: String,
  notableEvent: String
)

object NrsOrchestratorAttachmentWorkItem {
  def formatEncrypted(implicit
    jsonCrypto: Encrypter with Decrypter
  ): OFormat[NrsOrchestratorAttachmentWorkItem] = new OFormat[NrsOrchestratorAttachmentWorkItem] {
    private val nrSubmissionId = "nrSubmissionId"
    private val attachment = "attachment"
    private val businessId = "businessId"
    private val notableEvent = "notableEvent"
    override def writes(o: NrsOrchestratorAttachmentWorkItem): JsObject =
      JsObject(
        Seq(
          nrSubmissionId -> JsonCryptoUtils.encrypt(o.nrSubmissionId),
          attachment     -> JsonCryptoUtils.encrypt(o.attachment),
          businessId     -> JsonCryptoUtils.encrypt(o.businessId),
          notableEvent   -> JsonCryptoUtils.encrypt(o.notableEvent)
        )
      )

    override def reads(json: JsValue): JsResult[NrsOrchestratorAttachmentWorkItem] = {

      def decrypt[T: TypeTag](str: String)(implicit reads: Reads[T]) = JsonCryptoUtils.decrypt[T](json, str)
      val fields: Array[JsResult[_]] = Array(
        decrypt[String](nrSubmissionId),
        decrypt[NRSAttachment](attachment),
        decrypt[String](businessId),
        decrypt[String](notableEvent)
      )

      val validationErrors = fields.collect { case JsError(errors) => errors }.flatten

      if (validationErrors.isEmpty) {
        val validatedFields = fields.map(_.get)
        JsSuccess(
          NrsOrchestratorAttachmentWorkItem(
            validatedFields.head.asInstanceOf[String],
            validatedFields(1).asInstanceOf[NRSAttachment],
            validatedFields(2).asInstanceOf[String],
            validatedFields(3).asInstanceOf[String]
          )
        )
      } else {
        JsError(validationErrors)
      }
    }
  }
}
