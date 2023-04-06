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

package uk.gov.hmrc.gform.notificationbanner

import julienrf.json.derived
import play.api.libs.json.{ JsDefined, JsError, JsString, JsSuccess, JsUndefined, Json, OFormat }

case class NotificationBanner(
  message: String
)

object NotificationBanner {

  val mongoId = "notificationBanner"

  implicit val format: OFormat[NotificationBanner] = derived.oformat()

  val mongoFormat: OFormat[NotificationBanner] = OFormat[NotificationBanner](
    jsValue =>
      (jsValue \ "message") match {
        case JsDefined(JsString(message)) => JsSuccess(NotificationBanner(message))
        case JsDefined(unknown)           => JsError(s"Expected string for notification banner, got $unknown")
        case _: JsUndefined               => JsError(s"Missing field 'message' in json $jsValue")
      },
    (notificationBanner: NotificationBanner) =>
      Json.obj(
        "_id"     -> mongoId,
        "message" -> notificationBanner.message
      )
  )
}
