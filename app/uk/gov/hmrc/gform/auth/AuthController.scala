/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auth

import cats.implicits._
import play.api.libs.json.Json
import play.api.mvc.Action
import uk.gov.hmrc.gform.controllers.BaseController

class AuthController(
  authService: AuthService) extends BaseController {

  def check = Action.async(parse.json[String]) { implicit request =>
    authService.whiteListed(request.body).map {
      case Some(x) => Ok(Json.toJson(x.index))
      case None => NotFound
    }
  }

  def all = Action.async { implicit request =>
    authService.all().map(list => Ok(Json.toJson(list)))
  }

  def insert = Action.async(parse.json[String]) { implicit request =>
    authService.insert(request.body).map(_ => NoContent)
  }

  def delete = Action.async(parse.json[String]) { implicit request =>
    val result = for {
      x <- authService.delete(request.body)
    } yield x

    result.fold(
      errors => errors.asBadRequest,
      good => NoContent)
  }
}
