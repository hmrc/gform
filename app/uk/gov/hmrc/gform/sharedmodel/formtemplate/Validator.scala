/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.libs.functional.syntax._
import play.api.libs.functional.syntax._

import scala.concurrent.{ ExecutionContext, Future }

sealed trait Validator {
  def errorMessage: String
}

case object Validator {

  val reads: Reads[Validator] = Reads { json =>
    (json \ "validatorName").as[String] match {
      case "hmrcUTRPostcodeCheck"    => json.validate[HMRCUTRPostcodeCheckValidator]
      case "bankAccountModulusCheck" => json.validate[BankAccoutnModulusCheck]
    }
  }

  val writes: OWrites[Validator] = OWrites {
    case v: HMRCUTRPostcodeCheckValidator => HMRCUTRPostcodeCheckValidator.format.writes(v)
    case v: BankAccoutnModulusCheck       => BankAccoutnModulusCheck.format.writes(v)

  }

  implicit val format = OFormat(reads, writes)

}

case class HMRCUTRPostcodeCheckValidator(errorMessage: String, utr: FormCtx, postcode: FormCtx) extends Validator

object HMRCUTRPostcodeCheckValidator {
  private val basic: OFormat[HMRCUTRPostcodeCheckValidator] = Json.format[HMRCUTRPostcodeCheckValidator]
  private val writesCustom: OWrites[HMRCUTRPostcodeCheckValidator] = OWrites { o =>
    Json.obj("validatorName" -> "hmrcUTRPostcodeCheck") ++
      basic.writes(o)
  }

  private val writes: OWrites[HMRCUTRPostcodeCheckValidator] = writesCustom
  private val readCustom: Reads[HMRCUTRPostcodeCheckValidator] = ((JsPath \ "errorMessage").read[String] and
    (JsPath \ "parameters" \\ "utr").read[FormCtx] and
    (JsPath \ "parameters" \\ "postcode").read[FormCtx])(HMRCUTRPostcodeCheckValidator.apply _)

  private val reads = readCustom | (basic: Reads[HMRCUTRPostcodeCheckValidator])
  implicit val format: OFormat[HMRCUTRPostcodeCheckValidator] = OFormat(reads, writesCustom)
}

case class BankAccoutnModulusCheck(errorMessage: String, accountNumber: FormCtx, sortCode: FormCtx) extends Validator

object BankAccoutnModulusCheck {
  private val basic: OFormat[BankAccoutnModulusCheck] = Json.format[BankAccoutnModulusCheck]
  private val writesCustom: OWrites[BankAccoutnModulusCheck] = OWrites { o =>
    Json.obj("validatorName" -> "bankAccountModulusCheck") ++
      basic.writes(o)
  }

  private val writes: OWrites[BankAccoutnModulusCheck] = writesCustom
  private val readCustom: Reads[BankAccoutnModulusCheck] = ((JsPath \ "errorMessage").read[String] and
    (JsPath \ "parameters" \\ "accountNumber").read[FormCtx] and
    (JsPath \ "parameters" \\ "sortCode").read[FormCtx])(BankAccoutnModulusCheck.apply _)

  private val reads = readCustom | (basic: Reads[BankAccoutnModulusCheck])
  implicit val format: OFormat[BankAccoutnModulusCheck] = OFormat(reads, writesCustom)
}
