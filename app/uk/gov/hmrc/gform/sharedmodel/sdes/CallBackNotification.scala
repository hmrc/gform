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

package uk.gov.hmrc.gform.sharedmodel.sdes

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ADTFormat

final case class CallBackNotification(
  notification: NotificationStatus,
  filename: String,
  correlationID: String,
  failureReason: Option[String]
)

object CallBackNotification {
  implicit val format: OFormat[CallBackNotification] = {
    implicit val notificationStatusFormat: Format[NotificationStatus] = NotificationStatus.format
    derived.oformat()
  }
}

sealed trait NotificationStatus extends Product with Serializable

object NotificationStatus {

  case object NotNotified
      extends NotificationStatus //Indicates that the file couldn't be notified for any system reasons

  case object FileReady
      extends NotificationStatus //Indicates that the file specified in the notification is available to download from SDES

  case object FileReceived extends NotificationStatus //Indicates that the specified has been stored in SDES

  case object FileProcessingFailure extends NotificationStatus //The file specified has failed processing

  case object FileProcessed
      extends NotificationStatus //The file has passed all integrity checks and have been delivered to the recipient system in HMRC

  implicit val catsEq: Eq[NotificationStatus] = Eq.fromUniversalEquals

  implicit val format: Format[NotificationStatus] =
    ADTFormat.formatEnumeration(
      "NotNotified"           -> NotNotified,
      "FileReady"             -> FileReady,
      "FileReceived"          -> FileReceived,
      "FileProcessingFailure" -> FileProcessingFailure,
      "FileProcessed"         -> FileProcessed
    )

  def fromName(notificationStatus: NotificationStatus): String = notificationStatus match {
    case FileReady             => "FileReady"
    case FileReceived          => "FileReceived"
    case FileProcessingFailure => "FileProcessingFailure"
    case FileProcessed         => "FileProcessed"
  }
}
