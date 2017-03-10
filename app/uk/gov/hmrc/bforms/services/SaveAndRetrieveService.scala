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

package uk.gov.hmrc.bforms.services
import play.api.libs.json.{ JsObject, JsValue, Json }
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.models.{ DbOperationResult, SaveAndRetrieve }
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, Update }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object SaveService {
  def save(formData: SaveAndRetrieve, registrationNumber: String)(implicit save: Update[SaveAndRetrieve]): Future[Opt[DbOperationResult]] = {
    val selector = Json.obj("fields.id" -> "registrationNumber", "fields.value" -> registrationNumber)
    save(selector, formData)
  }
}

object RetrieveService {
  def retrieve(registrationNumber: String)(implicit find: FindOne[SaveAndRetrieve]): Future[Option[SaveAndRetrieve]] = {
    val selector = Json.obj("fields.id" -> "registrationNumber", "fields.value" -> registrationNumber)
    find(selector)
  }
}
