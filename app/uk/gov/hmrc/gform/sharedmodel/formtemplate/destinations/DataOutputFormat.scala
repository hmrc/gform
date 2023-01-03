/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.libs.json.{ JsError, JsString, JsSuccess, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

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

  val templateReads: Reads[DataOutputFormat] = Reads {
    case JsString("json") => JsSuccess(JSON)
    case JsString("xml")  => JsSuccess(XML)
    case JsString(err)    => JsError(s"dataOutputFormat on destination must be json or xml. $err is not allowed")
    case _                => JsError("Failure")
  }

  implicit val format: OFormat[DataOutputFormat] = OFormatWithTemplateReadFallback(templateReads)

}
