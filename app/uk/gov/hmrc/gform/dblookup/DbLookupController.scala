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

package uk.gov.hmrc.gform.dblookup

import play.api.libs.json.JsValue
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.UniqueIdGenerator
import uk.gov.hmrc.gform.sharedmodel.dblookup.{ CollectionName, DbLookupId }

import scala.concurrent.ExecutionContext

class DbLookupController(controllerComponents: ControllerComponents, dbLookupService: DbLookupService)(implicit
  ex: ExecutionContext,
  uniqueIdGenerator: UniqueIdGenerator
) extends BaseController(controllerComponents) {

  def exists(dbLookupId: DbLookupId, collectionName: CollectionName): Action[AnyContent] = Action.async { _ =>
    dbLookupService.find(dbLookupId, collectionName) map {
      case Some(_) => Ok
      case None    => NotFound
    }
  }

  def add(collectionName: CollectionName): Action[JsValue] = Action.async(parse.json) { implicit request =>
    val dbLookupIds = request.body.as[List[DbLookupId]]
    dbLookupService.addMulti(dbLookupIds, collectionName) map {
      case Right(_) => Created
      case Left(u)  => u.asInternalServerError
    }
  }

}
