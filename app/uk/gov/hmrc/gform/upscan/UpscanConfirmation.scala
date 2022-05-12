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

package uk.gov.hmrc.gform.upscan

import java.time.Instant
import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

case class UpscanConfirmation(
  _id: UpscanReference,
  status: UpscanFileStatus,
  confirmationFailure: ConfirmationFailure,
  confirmedAt: Instant
)

object UpscanConfirmation {
  implicit val format: OFormat[UpscanConfirmation] = {
    // This instance of Format[Instant] is making sure that 'confirmedAt' is stored as ISODate
    // so that TTL index is working https://docs.mongodb.com/manual/core/index-ttl/
    implicit val dtf: Format[Instant] = MongoJavatimeFormats.instantFormat
    derived.oformat()
  }
}

sealed trait ConfirmationFailure extends Product with Serializable

object ConfirmationFailure {
  case object AllOk extends ConfirmationFailure
  case class UpscanFailure(failureDetails: FailureDetails) extends ConfirmationFailure
  case class GformValidationFailure(validationFailure: UpscanValidationFailure) extends ConfirmationFailure

  implicit val format: Format[ConfirmationFailure] = derived.oformat()
}
