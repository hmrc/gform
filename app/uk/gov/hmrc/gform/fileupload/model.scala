/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.typeclasses.Now

case class Attachments(files: List[String]) {
  def size = files.size
}

object Attachments {
  val empty = Attachments(Nil)
  implicit val format = Json.format[Attachments]
}

case class FUConfig(
  fileUploadBaseUrl: String,
  fileUploadFrontendBaseUrl: String,
  expiryDays: Int,
  maxSize: String,
  maxSizePerItem: String,
  maxItems: Int,
  contentTypes: List[ContentType]
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

  implicit val format = OFormat(reads, owrites)
}

case class Envelope(files: List[File])

object Envelope {
  implicit val reads: Reads[Envelope] = envelopeRawReads.map(er => Envelope(er.files.getOrElse(Nil)))
  private lazy val envelopeRawReads = Json.reads[EnvelopeRaw]
}

case class EnvelopeRaw(files: Option[List[File]])
case class File(fileId: FileId, status: Status, fileName: String, length: Long)

object File {

  //TIP: look for FileStatus trait in https://github.com/hmrc/file-upload/blob/master/app/uk/gov/hmrc/fileupload/read/envelope/model.scala
  implicit val format: Reads[File] = fileRawReads.map {
    // format: OFF
    case FileRaw(id, name, "QUARANTINED", _, length)        => File(FileId(id), Quarantined, name, length)
    case FileRaw(id, name, "CLEANED", _, length)            => File(FileId(id), Cleaned, name, length)
    case FileRaw(id, name, "AVAILABLE", _, length)          => File(FileId(id), Available, name, length)
    case FileRaw(id, name, "INFECTED", _, length)           => File(FileId(id), Infected, name, length)
    case FileRaw(id, name, ERROR, Some(reason), length)     => File(FileId(id), Error(reason), name, length)
    case FileRaw(id, name, other, _, length)                => File(FileId(id), Other(other), name, length)
    // format: ON
  }
  private lazy val fileRawReads: Reads[FileRaw] = Json.reads[FileRaw]
  private lazy val ERROR = "UnKnownFileStatusERROR"
}

case class FileRaw(id: String, name: String, status: String, reason: Option[String], length: Long)

sealed trait Status
case object Quarantined extends Status
case object Infected extends Status
case object Cleaned extends Status
case object Available extends Status
case class Other(value: String) extends Status
case class Error(reason: String) extends Status
