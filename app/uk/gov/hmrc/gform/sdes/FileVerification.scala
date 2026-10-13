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

package uk.gov.hmrc.gform.sdes

import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ADTFormat

import java.time.Instant

case class FileVerification(
  fileId: Option[String],
  destination: Option[String],
  directory: String,
  fileName: String,
  exists: Boolean,
  contentLength: Option[Long],
  contentType: Option[String],
  lastModified: Option[Instant],
  source: FileVerificationSource
)

object FileVerification {
  implicit val format: OFormat[FileVerification] = Json.format[FileVerification]
}

case class EnvelopeVerification(
  envelopeId: String,
  fileList: List[FileVerification]
)

object EnvelopeVerification {
  implicit val format: OFormat[EnvelopeVerification] = Json.format[EnvelopeVerification]
}

sealed trait FileVerificationSource

object FileVerificationSource {
  case object Envelope extends FileVerificationSource
  case object SdesSubmission extends FileVerificationSource
  case object WorkItem extends FileVerificationSource
  case object ObjectStore extends FileVerificationSource

  implicit val format: Format[FileVerificationSource] =
    ADTFormat.formatEnumeration(
      "Envelope"       -> Envelope,
      "SdesSubmission" -> SdesSubmission,
      "WorkItem"       -> WorkItem,
      "ObjectStore"    -> ObjectStore
    )
}
