/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class Offset(value: Int) extends AnyVal

object Offset {

  val signedIntRegex = """(\+|-)?\d+"""

  implicit val offsetHelper: OFormat[Offset] = {
    val offsetExpr: OFormat[Offset] = Json.format[Offset]

    val convertToInt = (str: String) =>
      if (str.matches(signedIntRegex)) JsSuccess(Offset(str.toInt))
      else JsError(s"Couldn't parse Integer from offset, $str")

    val reads: Reads[Offset] = (offsetExpr: Reads[Offset]) | Reads {
      case JsString(offsetAsStr) =>
        convertToInt(offsetAsStr)

      case otherwise => JsError(s"Invalid format expression. Expected String, got $otherwise")
    }

    OFormat[Offset](reads, offsetExpr)
  }

}
