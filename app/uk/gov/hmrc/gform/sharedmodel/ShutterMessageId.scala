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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ Format, JsString, OFormat }

case class ShutterMessageId(value: String)

object ShutterMessageId {
  implicit val mongoVformat: Format[ShutterMessageId] =
    ValueClassFormat.vformat("_id", ShutterMessageId.apply, x => JsString(x.value))
  val mongoV2format: Format[ShutterMessageId] =
    ValueClassFormat.vformat("shutterMessageId", ShutterMessageId.apply, x => JsString(x.value))
  val oformat: OFormat[ShutterMessageId] = ValueClassFormat.oformat("shutterMessageId", ShutterMessageId.apply, _.value)
}
