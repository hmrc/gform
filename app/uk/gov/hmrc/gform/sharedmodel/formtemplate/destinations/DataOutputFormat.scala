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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import play.api.libs.json.{ Format, JsError, JsResult, JsString, JsSuccess, JsValue }

sealed trait DataOutputFormat {
  val content: String
}

object DataOutputFormat {
  case object JSON extends DataOutputFormat {
    override val content: String = "json"
  }
  case object XML extends DataOutputFormat {
    override val content: String = "xml"
  }

  implicit val format: Format[DataOutputFormat] = new Format[DataOutputFormat] {
    override def writes(o: DataOutputFormat): JsValue = o match {
      case JSON => JsString(JSON.content)
      case XML  => JsString(XML.content)
    }

    override def reads(json: JsValue): JsResult[DataOutputFormat] =
      json match {
        case JsString("json") => JsSuccess(JSON)
        case JsString("xml")  => JsSuccess(XML)
        case JsString(err)    => JsError(s"dataOutputFormat on destination must be json or xml. $err is not allowed")
        case _                => JsError("Failure")
      }
  }

}
