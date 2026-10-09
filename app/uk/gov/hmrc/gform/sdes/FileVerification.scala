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

import play.api.libs.json.{ Json, OFormat }

import java.time.Instant

case class FileVerification(
  fileId: Option[String],
  destination: Option[String],
  directory: String,
  fileName: String,
  exists: Boolean,
  contentLength: Option[Long] = None,
  contentType: Option[String] = None,
  lastModified: Option[Instant] = None,
  source: String
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
