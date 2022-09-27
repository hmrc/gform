/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.envelope

import cats.Show
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.UniqueIdGenerator
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }

case class EnvelopeData(
  _id: EnvelopeId,
  files: List[EnvelopeFile]
)

case class EnvelopeFile(fileId: FileId, fileName: String, status: FileStatus, length: Long)

object EnvelopeFile {
  val fileWrites: Writes[EnvelopeFile] =
    ((__ \ "id").write[FileId] and
      (JsPath \ "name").write[String] and
      (JsPath \ "status").write[FileStatus] and
      (JsPath \ "length").write[Long])(unlift(EnvelopeFile.unapply))

  val fileReads: Reads[EnvelopeFile] = (
    (JsPath \ "id").format[String].map(FileId(_)) and
      (JsPath \ "name").format[String] and
      (JsPath \ "status").format[FileStatus] and
      (JsPath \ "length").format[Long]
  )(EnvelopeFile.apply(_, _, _, _))

  implicit val fileFormat: Format[EnvelopeFile] = Format[EnvelopeFile](fileReads, fileWrites)
}

object EnvelopeData {

  val newEnvelope = EnvelopeData(EnvelopeId(UniqueIdGenerator.uuidStringGenerator.generate), List.empty[EnvelopeFile])

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
