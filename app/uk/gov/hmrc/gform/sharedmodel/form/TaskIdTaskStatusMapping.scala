/*
 * Copyright 2024 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ JsonUtils, TaskId }

case class TaskIdTaskStatusMapping(mapping: Map[TaskId, TaskStatus])

object TaskIdTaskStatusMapping {
  val empty = TaskIdTaskStatusMapping(Map.empty)
  val formatMap: Format[Map[TaskId, TaskStatus]] =
    JsonUtils.formatMap(TaskId.apply, _.id)

  implicit val format: Format[TaskIdTaskStatusMapping] = Format(
    formatMap.map(TaskIdTaskStatusMapping.apply),
    formatMap.contramap(_.mapping)
  )
}
