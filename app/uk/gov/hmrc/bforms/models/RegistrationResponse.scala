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

package uk.gov.hmrc.bforms.models

import play.api.i18n.Messages
import play.api.libs.json.{ JsObject, Json }

/**
 * Created by daniel-connelly on 17/01/17.
 */
sealed trait SaveResponse {

  private def error(msg: String) = Json.obj("error" -> msg)

  def toJson(messages: Messages): JsObject = this match {
    case RESPONSE_OK => Json.obj()
    case INVALID_DATA => error(messages("save.invalid.data"))
    case Other(msg) => error(msg)
  }

}

case object RESPONSE_OK extends SaveResponse
case object INVALID_DATA extends SaveResponse
case class Other(key: String) extends SaveResponse
