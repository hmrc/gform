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

import cats.Eq
import play.api.libs.json._

case class HandlebarsPayloadId(value: String) extends AnyVal

object HandlebarsPayloadId {
  implicit val catsEq: Eq[HandlebarsPayloadId] = Eq.fromUniversalEquals

  implicit val vformat: Format[HandlebarsPayloadId] =
    ValueClassFormat.vformat("handleBarsPayloadId", HandlebarsPayloadId.apply, x => JsString(x.value))

  implicit val oformat: OFormat[HandlebarsPayloadId] =
    ValueClassFormat.oformat("_id", HandlebarsPayloadId.apply, _.value)
}
