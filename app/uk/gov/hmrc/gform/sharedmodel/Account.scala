/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.json.{ Json, OFormat, OWrites, Reads }

case class Account(
  sortCode: String,
  accountNumber: String
)

object Account {
  val basic: OFormat[Account] = Json.format[Account]

  val writes: OWrites[Account] = OWrites[Account] { o =>
    Json.obj("account" -> Json.obj("sortCode" -> s"${o.sortCode.replaceAll("-", "")}", "accountNumber" -> s"${o.accountNumber}"))
  }

  val reads: Reads[Account] = basic

  implicit val format: OFormat[Account] = OFormat[Account](reads, writes)
}
