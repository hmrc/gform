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

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.config.AuthorizationName
import uk.gov.hmrc.gform.sharedmodel.{ PdfContent, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, HttpMethod, ProfileName }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue

/** AsyncHandlebarsRenderSnapshot Captures the submit-time render context for forward-only AsyncHandlebars regeneration.
  *
  * Regeneration reuses this captured model; it does not recompute expressionOutput or other form-template JSON changes
  * that would alter the resolved model. It is intended to pick up corrected AsyncHandlebars destination payload changes.
  */
case class AsyncHandlebarsRenderSnapshot(
  accumulatedModel: HandlebarsTemplateProcessorModel,
  model: HandlebarsTemplateProcessorModel,
  pdfData: PdfContent,
  instructionPdfData: Option[PdfContent],
  structuredFormData: StructuredFormValue.ObjectStructure,
  formId: FormId,
  submissionRef: SubmissionRef
)

object AsyncHandlebarsRenderSnapshot {
  implicit val formIdFormat: Format[FormId] = FormId.vformat
  implicit val submissionRefFormat: Format[SubmissionRef] = SubmissionRef.vformat
  implicit val format: OFormat[AsyncHandlebarsRenderSnapshot] = Json.format[AsyncHandlebarsRenderSnapshot]
}

case class AsyncHandlebarsWorkItem(
  profile: ProfileName,
  uri: String,
  method: HttpMethod,
  contentType: ContentType,
  payload: String,
  credential: Option[AuthorizationName],
  httpHeaders: Map[String, String] = Map.empty,
  renderSnapshot: Option[AsyncHandlebarsRenderSnapshot] = None
)

object AsyncHandlebarsWorkItem {
  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): OFormat[AsyncHandlebarsWorkItem] =
    new OFormat[AsyncHandlebarsWorkItem] {
      private val uri = "uri"
      private val method = "method"
      private val payload = "payload"
      private val credential = "credential"
      private val httpHeaders = "httpHeaders"
      private val renderSnapshot = "renderSnapshot"

      private def encrypt(value: String): JsString =
        JsString(jsonCrypto.encrypt(PlainText(value)).value)

      private def decrypt(value: String): String =
        jsonCrypto.decrypt(Crypted(value)).value

      override def writes(workItem: AsyncHandlebarsWorkItem): JsObject =
        ProfileName.oformat.writes(workItem.profile) ++
          Json.obj(uri -> workItem.uri) ++
          Json.obj(method -> workItem.method) ++
          ContentType.oformat.writes(workItem.contentType) ++
          Json.obj(payload -> encrypt(workItem.payload)) ++
          workItem.credential
            .map { workItemCredential =>
              Json.obj(credential -> JsString(workItemCredential.value))
            }
            .getOrElse(Json.obj()) ++
          Json.obj(httpHeaders -> workItem.httpHeaders) ++
          workItem.renderSnapshot
            .map { snapshot =>
              Json.obj(renderSnapshot -> encrypt(Json.toJson(snapshot).toString))
            }
            .getOrElse(Json.obj())

      override def reads(json: JsValue): JsResult[AsyncHandlebarsWorkItem] =
        for {
          profile     <- ProfileName.oformat.reads(json)
          uri         <- (json \ uri).validate[String]
          method      <- (json \ method).validate[HttpMethod]
          contentType <- ContentType.oformat.reads(json)
          payload     <- (json \ payload).validate[String].map(decrypt)
          credential <- (json \ credential)
                          .validateOpt[String]
                          .map(payload =>
                            payload.map { payload =>
                              AuthorizationName(payload)
                            }
                          )
          httpHeaders <- (json \ httpHeaders).validateOpt[Map[String, String]].map(_.getOrElse(Map.empty))
          renderSnapshot <- (json \ renderSnapshot)
                              .validateOpt[String]
                              .map(_.map(encrypted => Json.parse(decrypt(encrypted)).as[AsyncHandlebarsRenderSnapshot]))
        } yield AsyncHandlebarsWorkItem(
          profile,
          uri,
          method,
          contentType,
          payload,
          credential,
          httpHeaders,
          renderSnapshot
        )
    }
}
