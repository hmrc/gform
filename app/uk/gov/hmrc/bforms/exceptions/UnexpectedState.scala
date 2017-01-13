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

package uk.gov.hmrc.bforms.exceptions

import play.api.mvc.Results.BadRequest
import play.api.libs.json.{ Json, JsPath }

sealed trait UnexpectedState extends Product with Serializable {
  def message: String = this match {
    case InvalidState(errorMsg) => errorMsg
  }

  def toResult = {
    BadRequest(Json.obj("error" -> this.message))
  }
}

case class InvalidState(errorMsg: String) extends UnexpectedState
