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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }

case class FormComponentIdToFileIdMapping(mapping: Map[FormComponentId, FileId]) {
  def +(formComponentId: FormComponentId, fileId: FileId): FormComponentIdToFileIdMapping =
    FormComponentIdToFileIdMapping(mapping + (formComponentId -> fileId))
}

object FormComponentIdToFileIdMapping {
  val empty = FormComponentIdToFileIdMapping(Map.empty)
  val formatMap: Format[Map[FormComponentId, FileId]] = {
    implicit val fileIdFormat: Format[FileId] = JsonUtils.valueClassFormat[FileId, String](FileId.apply, _.value)
    JsonUtils.formatMap(FormComponentId.apply, _.value)
  }

  implicit val format: Format[FormComponentIdToFileIdMapping] = Format(
    formatMap.map(FormComponentIdToFileIdMapping.apply),
    formatMap.contramap(_.mapping)
  )
}
