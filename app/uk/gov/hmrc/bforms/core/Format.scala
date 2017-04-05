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
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
 * Created by dimitra on 03/04/17.
 */

sealed trait Format

final case class DateExpression(value: String) extends Format

object Format {
  implicit val format: OFormat[Format] = {
    val formatExpr: OFormat[Format] = derived.oformat

    val reads: Reads[Format] = (formatExpr: Reads[Format]) | Reads {
      case JsString(formatAsStr) =>
        FormatParser.validate(formatAsStr) match {
          case Right(expr) => JsSuccess(expr)
          case Left(error) => JsError(error.toString)
        }
      case otherwise => JsError(s"Invalid format expression. Expected String, got $otherwise")
    }

    OFormat[Format](reads, formatExpr)
  }
}
