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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.PresentationHintParser

sealed trait PresentationHint

case object InvisibleInSummary extends PresentationHint
case object SummariseGroupAsGrid extends PresentationHint
case object TotalValue extends PresentationHint
case object InvisiblePageTitle extends PresentationHint

object PresentationHint {
  implicit val format: OFormat[PresentationHint] = OFormatWithTemplateReadFallback {
    case JsString(str) => PresentationHintParser.validateSingle(str).fold(e => JsError(e.error), JsSuccess(_))
    case unknown       => JsError("Expected String, got " + unknown)
  }
}
