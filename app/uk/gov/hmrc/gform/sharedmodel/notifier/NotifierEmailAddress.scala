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

package uk.gov.hmrc.gform.sharedmodel.notifier

import cats.Show
import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils

case class NotifierEmailAddress(value: String) extends AnyVal

object NotifierEmailAddress {
  implicit val format: Format[NotifierEmailAddress] =
    JsonUtils.valueClassFormat[NotifierEmailAddress, String](NotifierEmailAddress.apply, _.value)

  implicit val show: Show[NotifierEmailAddress] = Show.show(_.value)
}
