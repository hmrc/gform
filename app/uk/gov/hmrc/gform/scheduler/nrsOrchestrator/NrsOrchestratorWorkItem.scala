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
import uk.gov.hmrc.gform.nrs.NrsPayload
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.{ NRSOrchestratorDestinationResultData, SubmissionRef }
import reflect.runtime.universe._

final case class NrsOrchestratorWorkItem(
  envelopeId: EnvelopeId,
  businessId: String,
  notableEvent: String,
  onSubmitHeaders: Seq[(String, String)],
  destinationResultData: NRSOrchestratorDestinationResultData,
  submissionRef: SubmissionRef,
  payload: NrsPayload,
  submissionDate: String,
  userAuthToken: String,
  identityData: JsObject
)

object NrsOrchestratorWorkItem {
  def formatEncrypted(implicit jsonCrypto: Encrypter with Decrypter): OFormat[NrsOrchestratorWorkItem] =
    new OFormat[NrsOrchestratorWorkItem] {
      private val envelopeId = "envelopeId"
      private val businessId = "businessId"
      private val notableEvent = "notableEvent"
      private val onSubmitHeaders = "onSubmitHeaders"
      private val destinationResultData = "destinationResultData"
      private val submissionRef = "submissionRef"
      private val payload = "payload"
      private val submissionDate = "submissionDate"
      private val userAuthToken = "userAuthToken"
      private val identityData = "identityData"
      override def writes(o: NrsOrchestratorWorkItem): JsObject =
        JsObject(
          Seq(
            envelopeId      -> JsonCryptoUtils.encrypt(o.envelopeId),
            businessId      -> JsonCryptoUtils.encrypt(o.businessId),
            notableEvent    -> JsonCryptoUtils.encrypt(o.notableEvent),
            onSubmitHeaders -> JsonCryptoUtils.encrypt(o.onSubmitHeaders),
            destinationResultData -> JsonCryptoUtils.encrypt(
              o.destinationResultData
            ),
            submissionRef  -> JsonCryptoUtils.encrypt(o.submissionRef),
            payload        -> JsonCryptoUtils.encrypt(o.payload),
            submissionDate -> JsonCryptoUtils.encrypt(o.submissionDate),
            userAuthToken  -> JsonCryptoUtils.encrypt(o.userAuthToken),
            identityData   -> JsonCryptoUtils.encrypt(o.identityData)
          )
        )

      override def reads(json: JsValue): JsResult[NrsOrchestratorWorkItem] = {

        def decrypt[T: TypeTag](str: String)(implicit reads: Reads[T]) = JsonCryptoUtils.decrypt(json, str)
        val fields: Array[JsResult[_]] = Array(
          decrypt[EnvelopeId](envelopeId),
          decrypt[String](businessId),
          decrypt[String](notableEvent),
          decrypt[Seq[(String, String)]](onSubmitHeaders),
          decrypt[NRSOrchestratorDestinationResultData](destinationResultData),
          decrypt[SubmissionRef](submissionRef),
          decrypt[NrsPayload](payload),
          decrypt[String](submissionDate),
          decrypt[String](userAuthToken),
          decrypt[JsObject](identityData)
        )

        val validationErrors = fields.collect { case JsError(errors) => errors }.flatten

        if (validationErrors.isEmpty) {
          val validatedFields = fields.map(_.get)
          JsSuccess(
            NrsOrchestratorWorkItem(
              validatedFields.head.asInstanceOf[EnvelopeId],
              validatedFields(1).asInstanceOf[String],
              validatedFields(2).asInstanceOf[String],
              validatedFields(3).asInstanceOf[Seq[(String, String)]],
              validatedFields(4).asInstanceOf[NRSOrchestratorDestinationResultData],
              validatedFields(5).asInstanceOf[SubmissionRef],
              validatedFields(6).asInstanceOf[NrsPayload],
              validatedFields(7).asInstanceOf[String],
              validatedFields(8).asInstanceOf[String],
              validatedFields(9).asInstanceOf[JsObject]
            )
          )
        } else {
          JsError(validationErrors)
        }
      }
    }
}
