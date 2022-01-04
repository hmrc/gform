/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.libs.json.{ Format, JsError, JsString, JsSuccess, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

final case class FormTemplateVersion(version: String) extends AnyVal

object FormTemplateVersion {

  private val templateReads: Reads[FormTemplateVersion] = Reads {
    case JsString(version) =>
      if (version.isEmpty) {
        JsError("'formVersion' cannot be empty")
      } else {
        JsSuccess(FormTemplateVersion(version))
      }
    case unknown => JsError(s"'formVersion' needs to be String, got $unknown")
  }

  implicit val oformat: OFormat[FormTemplateVersion] = OFormatWithTemplateReadFallback(templateReads)

  val vformat: Format[FormTemplateVersion] =
    ValueClassFormat
      .vformat[FormTemplateVersion]("formTemplateVersion", FormTemplateVersion.apply, x => JsString(x.version))
}
