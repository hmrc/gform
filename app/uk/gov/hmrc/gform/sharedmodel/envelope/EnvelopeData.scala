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

package uk.gov.hmrc.gform.sharedmodel.envelope

import cats.Show
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.UniqueIdGenerator
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import scala.annotation.nowarn

case class EnvelopeData(
  _id: EnvelopeId,
  files: List[EnvelopeFile]
)

case class EnvelopeFile(
  fileId: String,
  fileName: String,
  status: FileStatus,
  contentType: ContentType,
  length: Long,
  metadata: Map[String, List[String]]
)

object EnvelopeFile {
  private val fileWrites: Writes[EnvelopeFile] =
    ((__ \ "id").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "status").write[FileStatus] and
      (JsPath \ "contentType").write[ContentType] and
      (JsPath \ "length").write[Long] and
      (JsPath \ "metadata").write[Map[String, List[String]]])(unlift(EnvelopeFile.unapply))

  private val fileReads: Reads[EnvelopeFile] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "status").read[FileStatus] and
      (JsPath \ "contentType").read[ContentType] and
      (JsPath \ "length").read[Long] and
      (JsPath \ "metadata").read[Map[String, List[String]]]
  )(EnvelopeFile.apply _)

  implicit val fileFormat: Format[EnvelopeFile] = Format[EnvelopeFile](fileReads, fileWrites)
}

object EnvelopeData {

  def createEnvelope =
    EnvelopeData(EnvelopeId(UniqueIdGenerator.uuidStringGenerator.generate), List.empty[EnvelopeFile])

  implicit val show: Show[EnvelopeData] = Show.show(_._id.value)

  private val reads: Reads[EnvelopeData] = (
    (EnvelopeId.oformat: Reads[EnvelopeId]) and
      (JsPath \ "files").read[List[EnvelopeFile]]
  )(EnvelopeData.apply _)

  private val writes: OWrites[EnvelopeData] = OWrites[EnvelopeData](s =>
    EnvelopeId.oformat.writes(s._id) ++
      Json.obj("files" -> s.files.map(EnvelopeFile.fileFormat.writes(_)))
  )

  implicit val format: OFormat[EnvelopeData] = OFormat[EnvelopeData](reads, writes)
}

sealed trait FileStatus {
  val value: String
}

case object Quarantined extends FileStatus {
  val value = "QUARANTINED"
}

case object Cleaned extends FileStatus {
  val value = "CLEANED"
}

case object Available extends FileStatus {
  val value = "AVAILABLE"
}

case object Infected extends FileStatus {
  val value = "INFECTED"
}

object FileStatus {

  def unapply(status: FileStatus): String = status.value

  @nowarn
  val reads: Reads[FileStatus] = for {
    value <- JsPath.read[String].map {
               case Quarantined.value => Quarantined
               case Cleaned.value     => Cleaned
               case Available.value   => Available
               case Infected.value    => Infected
             }
  } yield value

  val writes: Writes[FileStatus] = Writes { status: FileStatus =>
    JsString(status.value)
  }

  implicit val format: Format[FileStatus] = Format(reads, writes)
}
