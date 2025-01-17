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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json.Format
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }

case class FormComponentIdToFileIdMapping(
  // FileComponentId - it does change when atl iteration is removed or when file from multi-file is removed, so FileComponentId will never contains a gap in their sequence
  // FileId - it does never change, it is either created or deleted, so there can occurs a gap in their sequence (gap can exists
  //          because we cannot rename file in object-store)
  mapping: Map[FileComponentId, FileId]
) {
  def +(fileComponentId: FileComponentId, fileId: FileId): FormComponentIdToFileIdMapping =
    FormComponentIdToFileIdMapping(mapping + (fileComponentId -> fileId))

  def nextIndex(formComponentId: FormComponentId): Int =
    1 + mapping.count { case (k, v) => k.value().endsWith(formComponentId.value) }
}

object FormComponentIdToFileIdMapping {
  val empty = FormComponentIdToFileIdMapping(Map.empty)
  val formatMap: Format[Map[FileComponentId, FileId]] = {
    implicit val fileIdFormat: Format[FileId] = JsonUtils.valueClassFormat[FileId, String](FileId.apply, _.value)
    JsonUtils.formatMap(FileComponentId.fromString(_), _.value())
  }

  implicit val format: Format[FormComponentIdToFileIdMapping] = Format(
    formatMap.map(FormComponentIdToFileIdMapping.apply),
    formatMap.contramap(_.mapping)
  )
}
