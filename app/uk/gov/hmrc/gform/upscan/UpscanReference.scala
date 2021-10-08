/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json.{ Format, JsString }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

final case class UpscanReference(value: String) extends AnyVal

object UpscanReference {
  implicit val mongoVformat: Format[UpscanReference] =
    ValueClassFormat.vformat("_id", UpscanReference.apply, x => JsString(x.value))
}
