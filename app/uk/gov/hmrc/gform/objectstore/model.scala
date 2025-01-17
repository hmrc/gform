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

package uk.gov.hmrc.gform.objectstore

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.typeclasses.Now

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class Attachments(files: List[String]) {
  def size = files.size
}

object Attachments {
  val empty = Attachments(Nil)
  implicit val format: OFormat[Attachments] = Json.format[Attachments]
}

case class FUConfig(
  fileUploadBaseUrl: String,
  fileUploadFrontendBaseUrl: String,
  expiryDays: Int,
  maxSize: String,
  maxSizePerItem: String,
  maxItems: Int
)

class SpoiltLocationHeader(val message: String) extends RuntimeException(message)

case class ReconciliationId(value: String) extends AnyVal {
  override def toString = value
}

object ReconciliationId {

  def create(submissionRef: SubmissionRef)(implicit now: Now[LocalDateTime]): ReconciliationId = {
    val dateFormatter = now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
    ReconciliationId(submissionRef.withoutHyphens + "-" + dateFormatter)
  }
}

case class RouteEnvelopeRequest(envelopeId: EnvelopeId, application: String, destination: String)

object RouteEnvelopeRequest {

  private val macrowrites = Json.writes[RouteEnvelopeRequest]

  val owrites = OWrites[RouteEnvelopeRequest] { r =>
    macrowrites.writes(r) ++
      EnvelopeId.format.writes(r.envelopeId) //this will override envelopeId
  }

  val reads = Json.reads[RouteEnvelopeRequest]

  implicit val format: OFormat[RouteEnvelopeRequest] = OFormat(reads, owrites)
}

case class Envelope(files: List[File])

object Envelope {
  private val envelopeRawReads = Json.reads[EnvelopeRaw]
  implicit val reads: Reads[Envelope] = envelopeRawReads.map(er => Envelope(er.files.getOrElse(Nil)))
}

case class EnvelopeRaw(files: Option[List[File]])
case class File(
  fileId: FileId,
  status: FileStatus,
  fileName: String,
  length: Long
)

object File {
  implicit val fileRawReads: Reads[File] = Json.reads[File]
}

case class FileRaw(id: String, name: String, status: String, reason: Option[String], length: Long)
