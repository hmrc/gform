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
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HttpMethod, ProfileName }

case class AsyncHandlebarsWorkItem(
  envelopeId: EnvelopeId,
  formTemplateId: FormTemplateId,
  submissionRef: SubmissionRef,
  profile: ProfileName,
  uri: String,
  method: HttpMethod,
  contentType: String,
  payloads: List[String]
)

object AsyncHandlebarsWorkItem {
  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): OFormat[AsyncHandlebarsWorkItem] =
    new OFormat[AsyncHandlebarsWorkItem] {
      private val envelopeId = "envelopeId"
      private val formTemplateId = "formTemplateId"
      private val submissionRef = "submissionRef"
      private val profile = "profile"
      private val uri = "uri"
      private val method = "method"
      private val contentType = "contentType"
      private val payloads = "payloads"

      override def writes(workItem: AsyncHandlebarsWorkItem): JsObject =
        EnvelopeId.format.writes(workItem.envelopeId) ++
          FormTemplateId.oformat.writes(workItem.formTemplateId) ++
          SubmissionRef.oformat.writes(workItem.submissionRef) ++
          ProfileName.oformat.writes(workItem.profile) ++
          Json.obj(uri -> workItem.uri) ++
          Json.obj(method -> workItem.method) ++
          Json.obj(contentType -> workItem.contentType) ++
          Json.obj(
            payloads -> JsArray(
              workItem.payloads.map(str => JsString(jsonCrypto.encrypt(PlainText(Json.toJson(str).toString())).value))
            )
          )

      override def reads(json: JsValue): JsResult[AsyncHandlebarsWorkItem] =
        for {
          envelopeId     <- (json \ envelopeId).validate[EnvelopeId](EnvelopeId.oformat)
          formTemplateId <- (json \ formTemplateId).validate[FormTemplateId](FormTemplateId.oformat)
          submissionRef  <- (json \ submissionRef).validate[SubmissionRef](SubmissionRef.oformat)
          profile        <- (json \ profile).validate[ProfileName](ProfileName.oformat)
          uri            <- (json \ uri).validate[String]
          method         <- (json \ method).validate[HttpMethod]
          contentType    <- (json \ contentType).validate[String]
          payloads <- (json \ payloads)
                        .validate[List[String]]
                        .map(_.map(payload => jsonCrypto.decrypt(Crypted(payload)).value))
        } yield AsyncHandlebarsWorkItem(
          envelopeId,
          formTemplateId,
          submissionRef,
          profile,
          uri,
          method,
          contentType,
          payloads
        )
    }
}
