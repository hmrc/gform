/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.Eq
import play.api.libs.json.{ JsError, JsString, JsSuccess, Json, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

sealed trait InstructionPdfFields extends Product with Serializable

object InstructionPdfFields {

  case object All extends InstructionPdfFields
  case object Ordered extends InstructionPdfFields

  implicit val equal: Eq[InstructionPdfFields] = Eq.fromUniversalEquals

  val reads: Reads[InstructionPdfFields] = Reads {
    case JsString("all")     => JsSuccess(InstructionPdfFields.All)
    case JsString("ordered") => JsSuccess(InstructionPdfFields.Ordered)
    case otherwise =>
      val got = otherwise match {
        case JsString(str) => s"'$str'"
        case _             => Json.prettyPrint(otherwise)
      }
      JsError("Invalid value for instructionPdfFields. Expected json String 'all' or 'ordered', but got: " + got)
  }

  implicit val format: OFormat[InstructionPdfFields] = OFormatWithTemplateReadFallback(reads)
}
