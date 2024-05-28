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

package uk.gov.hmrc.gform.fileupload

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import org.apache.pekko.actor.Scheduler
import org.apache.pekko.pattern.after
import org.slf4j.LoggerFactory
import play.api.http.HeaderNames.LOCATION
import play.api.libs.json.{ JsObject, Json }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowedFileTypes, FormTemplateId, JsonUtils }
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ ExecutionContext, Future }

class Helper(config: FUConfig) extends JsonUtils {

  def createEnvelopeRequestBody(
    formTemplateId: FormTemplateId,
    allowedFileTypes: AllowedFileTypes,
    expiryDate: LocalDateTime,
    fileSizeLimit: Option[Int]
  ): JsObject =
    Json.obj(
      "constraints" -> Json.obj(
        "contentTypes"   -> Json.toJson(allowedFileTypes.contentTypes),
        "maxItems"       -> config.maxItems,
        "maxSize"        -> config.maxSize,
        "maxSizePerItem" -> s"${fileSizeLimit.getOrElse(config.maxSizePerItem)}MB"
      ),
      "expiryDate" -> envelopeExpiryDate(expiryDate),
      "metadata"   -> Json.obj("application" -> "gform", "formTemplateId" -> s"${formTemplateId.value}")
    )

  /** There must be Location header. If not this is exceptional situation!
    */
  def extractEnvelopId(resp: HttpResponse): EnvelopeId = resp.header(LOCATION) match {
    case Some(EnvelopeIdExtractor(envelopeId)) => EnvelopeId(envelopeId)
    case Some(location)                        => throw new SpoiltLocationHeader(location)
    case _                                     => throw new SpoiltLocationHeader(s"Header $LOCATION not found")
  }

  private lazy val EnvelopeIdExtractor = "envelopes/([\\w\\d-]+)$".r.unanchored
  private[fileupload] def envelopeExpiryDate(expiryDate: LocalDateTime) = expiryDate.format(formatter)
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
}

trait Retrying {

  private val logger = LoggerFactory.getLogger(getClass)

  def retry[T](f: => Future[T], delays: List[FiniteDuration], msg: String)(implicit
    ec: ExecutionContext,
    s: Scheduler
  ): Future[T] =
    f recoverWith { case t =>
      delays match {
        case Nil =>
          logger.warn(s"Giving up: $msg")
          Future.failed(t)
        case delay :: rest =>
          logger.warn(s"Retrying after $delay: $msg")
          after(delay, s)(retry(f, rest, msg))
      }
    }
}
