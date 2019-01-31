/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileupload

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{ JsObject, Json }
import play.api.libs.json.Writes.DefaultLocalDateTimeWrites
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeExpiryDate, EnvelopeId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HttpResponse

class Helper(config: FUConfig) {

  def createEnvelopeRequestBody(formTemplateId: FormTemplateId, expiryDate: LocalDateTime): JsObject =
    Json.obj(
      "constraints" -> Json.obj(
        "contentTypes"   -> contentTypesJson,
        "maxItems"       -> config.maxItems,
        "maxSize"        -> config.maxSize,
        "maxSizePerItem" -> config.maxSizePerItem),
      "expiryDate" -> s"${envelopeExpiryDate(expiryDate)}",
      "metadata"   -> Json.obj("application" -> "gform", "formTemplateId" -> s"${formTemplateId.value}")
    )

  /**
    * There must be Location header. If not this is exceptional situation!
    */
  def extractEnvelopId(resp: HttpResponse): EnvelopeId = resp.header(LOCATION) match {
    case Some(EnvelopeIdExtractor(envelopeId)) => EnvelopeId(envelopeId)
    case Some(location)                        => throw new SpoiltLocationHeader(location)
    case _                                     => throw new SpoiltLocationHeader(s"Header $LOCATION not found")
  }

  private lazy val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  private[fileupload] def envelopeExpiryDate(expiryDate: LocalDateTime) = expiryDate.format(formatter)
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  private lazy val contentTypesJson = Json.toJson(config.contentTypes)
}
