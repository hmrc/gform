/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json._

sealed trait DateValue
final case object TodayDateValue extends DateValue
final case class ExactDateValue(year: Int, month: Int, day: Int) extends DateValue
final case class NextDateValue(month: Int, day: Int) extends DateValue
final case class PreviousDateValue(month: Int, day: Int) extends DateValue

object DateValue {
  implicit val format: OFormat[DateValue] = derived.oformat
}
