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

package uk.gov.hmrc.gform.exceptions

import play.api.libs.json.Json
import uk.gov.hmrc.gform.controllers.ErrResponse
import play.api.mvc.Results._

case class UnexpectedState(error: String) {
  def asBadRequest = BadRequest(Json.toJson(ErrResponse(error)))
}
//
//sealed trait UnexpectedState {
//
//  private def toMessage(msg: String) = Json.obj("error" -> msg)
//
//  def asJson: JsObject = this match {
//    case UnexpectedState(errorMsg) => toMessage(errorMsg)
//    case UnexpectedStateWithJson(errorMsg, json) => toMessage(errorMsg) + ("json" -> json)
//  }
//
//  def toResult = Ok(asJson)
//
//  override def toString = this match {
//    case UnexpectedState(errorMsg) => errorMsg
//    case UnexpectedStateWithJson(errorMsg, json) => errorMsg + " " + Json.stringify(json)
//  }
//}
//
//case class UnexpectedState(errorMsg: String) extends UnexpectedState
//case class UnexpectedStateWithJson(errorMsg: String, json: JsValue) extends UnexpectedState
