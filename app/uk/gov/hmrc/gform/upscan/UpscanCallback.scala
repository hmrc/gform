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

package uk.gov.hmrc.gform.upscan

import java.time.Instant
import julienrf.json.derived
import play.api.libs.json.{ Format, JsError, JsResult, JsString, JsSuccess, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.config.ContentType

sealed trait UpscanCallback

object UpscanCallback {
  final case class Success(
    reference: UpscanReference,
    downloadUrl: String,
    fileStatus: UpscanFileStatus,
    uploadDetails: UploadDetails
  ) extends UpscanCallback

  final case class Failure(
    reference: UpscanReference,
    fileStatus: UpscanFileStatus,
    failureDetails: FailureDetails
  ) extends UpscanCallback

  implicit val successReads: Reads[Success] = derived.reads()
  implicit val failureReads: Reads[Failure] = derived.reads()

  implicit val upscanCallbackReads: Reads[UpscanCallback] = Reads { json =>
    json.validate[Success].orElse(json.validate[Failure])
  }
}

final case class UploadDetails(
  uploadTimestamp: Instant,
  checksum: String,
  fileMimeType: String,
  fileName: String,
  size: Long
)

object UploadDetails {
  implicit val reads: Reads[UploadDetails] = derived.reads()
}

sealed trait UpscanFileStatus

object UpscanFileStatus {
  case object Ready extends UpscanFileStatus
  case object Failed extends UpscanFileStatus

  implicit val format: Format[UpscanFileStatus] = new Format[UpscanFileStatus] {
    override def writes(o: UpscanFileStatus): JsValue = o match {
      case Ready  => JsString("ready")
      case Failed => JsString("failed")
    }

    override def reads(json: JsValue): JsResult[UpscanFileStatus] =
      json match {
        case JsString(status) =>
          status.toLowerCase match {
            case "ready"  => JsSuccess(Ready)
            case "failed" => JsSuccess(Failed)
            case unknown  => JsError(s"Unknown upscan file status. Expected 'ready' or 'failed', but got: $unknown")
          }
        case unknown => JsError(s"Unknown upscan file status. Expected JsString, but got: $unknown")

      }
  }
}

final case class FailureDetails(
  failureReason: String,
  message: String
)

object FailureDetails {
  implicit val format: OFormat[FailureDetails] = derived.oformat()
}

sealed trait UpscanValidationFailure extends Product with Serializable

object UpscanValidationFailure {
  case object EntityTooLarge extends UpscanValidationFailure
  case object EntityTooSmall extends UpscanValidationFailure
  case class InvalidFileType(errorDetail: String, fileMimeType: ContentType) extends UpscanValidationFailure

  implicit val format: Format[UpscanValidationFailure] = derived.oformat()
}
