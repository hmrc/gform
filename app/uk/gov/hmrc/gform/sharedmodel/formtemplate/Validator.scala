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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.libs.json._
import play.api.libs.json.Reads._
import uk.gov.hmrc.gform.core.parsers.ExprParsers
import uk.gov.hmrc.play.http.HeaderCarrier
import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._

import scala.concurrent.{ ExecutionContext, Future }

sealed trait Validator {
  def errorMessage: String
}

case object Validator {

  val reads: Reads[Validator] = Reads { json =>
    (json \ "validatorName").as[String] match {
      case "hmrcUTRPostcodeCheck" => json.validate[HMRCUTRPostcodeCheckValidator]
    }
  }

  val writes: OWrites[Validator] = OWrites {
    case v: HMRCUTRPostcodeCheckValidator => HMRCUTRPostcodeCheckValidator.format.writes(v)

  }

  implicit val format = OFormat(reads, writes)

}

case class HMRCUTRPostcodeCheckValidator(errorMessage: String, utr: FormCtx, postcode: FormCtx) extends Validator {

  val utrFieldId = FieldId(utr.value)
  val postcodeFieldId = FieldId(postcode.value)

}

object HMRCUTRPostcodeCheckValidator {
  val basic: OFormat[HMRCUTRPostcodeCheckValidator] = Json.format[HMRCUTRPostcodeCheckValidator]
  val writesCustom: OWrites[HMRCUTRPostcodeCheckValidator] = OWrites { o =>
    Json.obj("validatorName" -> "hmrcUTRPostcodeCheck") ++
      basic.writes(o)
  }

  val writes: OWrites[HMRCUTRPostcodeCheckValidator] = writesCustom
  val readCustom: Reads[HMRCUTRPostcodeCheckValidator] = ((JsPath \ "errorMessage").read[String] and
    (JsPath \ "parameters" \\ "utr").read[FormCtx] and
    (JsPath \ "parameters" \\ "postcode").read[FormCtx])(HMRCUTRPostcodeCheckValidator.apply _)

  val reads = readCustom | (basic: Reads[HMRCUTRPostcodeCheckValidator])
  implicit val format: OFormat[HMRCUTRPostcodeCheckValidator] = OFormat(reads, writesCustom)
}
