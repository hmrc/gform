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

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.LocalisedString

case class Save4LaterInfoText(value: LocalisedString) extends AnyVal

object Save4LaterInfoText {
  val templateReads: Reads[Save4LaterInfoText] = LocalisedString.format.map(Save4LaterInfoText(_))

  implicit val format: OFormat[Save4LaterInfoText] = OFormatWithTemplateReadFallback(templateReads)

}
