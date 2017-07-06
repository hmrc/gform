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

package uk.gov.hmrc.gform.services

import cats.instances.future._
import play.api.libs.json.Json
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses.{ Find, FindOne, Insert, Update }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FormTemplateService {

  def get(formTypeId: FormTypeId, version: Version)(implicit FindOne: FindOne[FormTemplate]): ServiceResponse[FormTemplate] = {
    val selector = Json.obj(
      "formTypeId" -> formTypeId.value,
      "version" -> version.value
    )
    val template: Future[Option[FormTemplate]] = FindOne(selector)
    fromFutureOptionA(template)(InvalidState(s"FormTemplate for (version: ${version.value}, formTypeId: ${formTypeId.value}) not found"))
  }
}
