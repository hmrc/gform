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

package uk.gov.hmrc.gform.scheduler.asynchandlebars

import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationId, HttpMethod, ProfileName }
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId

case class AsyncHandlebarsWorkItem(
  envelopeId: EnvelopeId,
  correlationId: CorrelationId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  destinationId: DestinationId,
  profile: ProfileName,
  uri: String,
  method: HttpMethod,
  contentType: ContentType,
  payload: String
)

object AsyncHandlebarsWorkItem {
  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): OFormat[AsyncHandlebarsWorkItem] =
    new OFormat[AsyncHandlebarsWorkItem] {
      private val uri = "uri"
      private val method = "method"
      private val payload = "payload"

      override def writes(workItem: AsyncHandlebarsWorkItem): JsObject =
        EnvelopeId.format.writes(workItem.envelopeId) ++
          CorrelationId.oformat.writes(workItem.correlationId) ++
          FormTemplateId.oformat.writes(workItem.formTemplateId) ++
          SubmissionRef.oformat.writes(workItem.submissionRef) ++
          DestinationId.oformat.writes(workItem.destinationId) ++
          ProfileName.oformat.writes(workItem.profile) ++
          Json.obj(uri -> workItem.uri) ++
          Json.obj(method -> workItem.method) ++
          ContentType.oformat.writes(workItem.contentType) ++
          Json.obj(payload -> JsString(jsonCrypto.encrypt(PlainText(workItem.payload)).value))

      override def reads(json: JsValue): JsResult[AsyncHandlebarsWorkItem] =
        for {
          envelopeId     <- EnvelopeId.format.reads(json)
          correlationId  <- CorrelationId.oformat.reads(json)
          formTemplateId <- FormTemplateId.oformat.reads(json)
          submissionRef  <- SubmissionRef.oformat.reads(json)
          destinationId  <- DestinationId.oformat.reads(json)
          profile        <- ProfileName.oformat.reads(json)
          uri            <- (json \ uri).validate[String]
          method         <- (json \ method).validate[HttpMethod]
          contentType    <- ContentType.oformat.reads(json)
          payload        <- (json \ payload).validate[String].map(payload => jsonCrypto.decrypt(Crypted(payload)).value)
        } yield AsyncHandlebarsWorkItem(
          envelopeId,
          correlationId,
          formTemplateId,
          submissionRef,
          destinationId,
          profile,
          uri,
          method,
          contentType,
          payload
        )
    }
}
