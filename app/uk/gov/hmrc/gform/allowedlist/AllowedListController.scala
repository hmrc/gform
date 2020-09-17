/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.allowedlist

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.allowedlist.AllowedListName.{ AllowedListName, mtdVatNumber }
import uk.gov.hmrc.gform.controllers.{ BaseController, ErrResponse }
import uk.gov.hmrc.gform.core.{ FOpt, UniqueIdGenerator }
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.allowedlist.APIMTDVatNumber

import scala.concurrent.ExecutionContext
import cats.implicits._

class AllowedListController(
  mtdVatNumberRepo: RepoAlgebra[MTDVatNumber, FOpt],
  controllerComponents: ControllerComponents)(implicit ex: ExecutionContext, uniqueIdGenerator: UniqueIdGenerator)
    extends BaseController(controllerComponents) {
  def add(allowedListName: AllowedListName) = Action.async(parse.json) { implicit request =>
    allowedListName match {
      case `mtdVatNumber` =>
        mtdVatNumberRepo
          .upsertBulk(request.body.as[List[APIMTDVatNumber]].map(_.toMTDVatNumber))
          .fold(u => u.asInternalServerError, _ => Created(""))
    }
  }
}
