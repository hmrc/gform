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

package uk.gov.hmrc.gform.sharedmodel

import cats.Eq
import play.api.libs.json.Format

case class PdfHtml(html: String) extends AnyVal

object PdfHtml {
  implicit val format: Format[PdfHtml] = ValueClassFormat.simpleFormat[PdfHtml](PdfHtml(_))(_.html)

  implicit val equal: Eq[PdfHtml] = Eq.fromUniversalEquals
}
