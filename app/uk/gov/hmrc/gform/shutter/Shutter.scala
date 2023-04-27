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

package uk.gov.hmrc.gform.shutter

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.ShutterMessageId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class Shutter(
  _id: ShutterMessageId,
  message: String
)

object Shutter {
  implicit val format: OFormat[Shutter] = derived.oformat()
}

case class ShutterView(
  _id: ShutterMessageId,
  message: String,
  formTemplateIds: List[FormTemplateId]
)

object ShutterView {
  implicit val format: OFormat[ShutterView] = derived.oformat()
}
