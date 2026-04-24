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

package uk.gov.hmrc.gform.submission

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId

import java.time.{ Instant, LocalDateTime, ZoneOffset }
import java.util.UUID

case class WorkItemHistory(
  _id: UUID,
  createdAt: Instant,
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  destinationId: DestinationId,
  responseStatus: Int,
  responseBody: String
)

object WorkItemHistory {
  def create(
    envelopeId: EnvelopeId,
    formTemplateId: FormTemplateId,
    submissionRef: SubmissionRef,
    destinationId: DestinationId,
    responseStatus: Int,
    responseBody: String
  ): WorkItemHistory =
    WorkItemHistory(
      UUID.randomUUID,
      LocalDateTime.now().toInstant(ZoneOffset.UTC),
      envelopeId,
      formTemplateId,
      submissionRef,
      destinationId,
      responseStatus,
      responseBody
    )

  val format: OFormat[WorkItemHistory] = {
    implicit val envelopeIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
    implicit val formTemplateIdFormat: Format[FormTemplateId] = FormTemplateId.vformat
    implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
    implicit val destinationIdFormat: Format[DestinationId] = DestinationId.format
    derived.oformat()
  }

  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): OFormat[WorkItemHistory] =
    new OFormat[WorkItemHistory] {
      private val responseBodyField = "responseBody"

      override def writes(history: WorkItemHistory): JsObject =
        format.writes(history) ++ Json.obj(
          responseBodyField -> JsString(jsonCrypto.encrypt(PlainText(history.responseBody)).value)
        )

      override def reads(json: JsValue): JsResult[WorkItemHistory] =
        for {
          history               <- format.reads(json)
          encryptedResponseBody <- (json \ responseBodyField).validate[String]
        } yield history.copy(responseBody = jsonCrypto.decrypt(Crypted(encryptedResponseBody)).value)
    }
}
