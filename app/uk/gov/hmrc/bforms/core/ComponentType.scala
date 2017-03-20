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

package uk.gov.hmrc.bforms.core

import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
 * Created by dimitra on 20/03/17.
 */
sealed trait ComponentType

case object Text extends ComponentType

case object Date extends ComponentType

case object Address extends ComponentType

object ComponentType {

  implicit val format: OFormat[ComponentType] = {
    val formatExpr: OFormat[ComponentType] = derived.oformat

    val reads: Reads[ComponentType] = (formatExpr: Reads[ComponentType]) | Reads {
      case JsString("text") => JsSuccess(Text)
      case JsString("date") => JsSuccess(Date)
      case JsString("address") => JsSuccess(Address)
      case other => JsError(s"Expected Text or Date as String, you entered: $other")
    }

    OFormat[ComponentType](reads, formatExpr)
  }

}
