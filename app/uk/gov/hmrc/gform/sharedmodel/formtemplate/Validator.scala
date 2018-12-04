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

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

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

  implicit val format: OFormat[Validator] = OFormat(reads, writes)

}

case class HMRCUTRPostcodeCheckValidator(errorMessage: String, utr: FormCtx, postcode: FormCtx) extends Validator

object HMRCUTRPostcodeCheckValidator {
  private val writes: OWrites[HMRCUTRPostcodeCheckValidator] = OWrites { o =>
    Json.obj(
      "validatorName" -> "hmrcUTRPostcodeCheck",
      "errorMessage"  -> o.errorMessage,
      "parameters" -> Json.obj(
        "utr"      -> FormCtx.simpleDollarWrites.writes(o.utr),
        "postcode" -> FormCtx.simpleDollarWrites.writes(o.postcode)
      )
    )
  }

  private val reads: Reads[HMRCUTRPostcodeCheckValidator] =
    ((JsPath \ "errorMessage").read[String] and
      (JsPath \ "parameters" \ "utr").read(FormCtx.simpleDollarReads) and
      (JsPath \ "parameters" \ "postcode").read(FormCtx.simpleDollarReads))(HMRCUTRPostcodeCheckValidator.apply _)

  implicit val format: OFormat[HMRCUTRPostcodeCheckValidator] = OFormat(reads, writes)
}

case class BankAccoutnModulusCheck(errorMessage: String, accountNumber: FormCtx, sortCode: FormCtx) extends Validator

object BankAccoutnModulusCheck {
  private val writes: OWrites[BankAccoutnModulusCheck] = OWrites { o =>
    Json.obj(
      "validatorName" -> "bankAccountModulusCheck",
      "errorMessage"  -> o.errorMessage,
      "parameters" -> Json.obj(
        "accountNumber" -> FormCtx.simpleDollarWrites.writes(o.accountNumber),
        "sortCode"      -> FormCtx.simpleDollarWrites.writes(o.sortCode)
      )
    )
  }

  private val reads: Reads[BankAccoutnModulusCheck] =
    ((JsPath \ "errorMessage").read[String] and
      (JsPath \ "parameters" \ "accountNumber").read(FormCtx.simpleDollarReads) and
      (JsPath \ "parameters" \ "sortCode").read(FormCtx.simpleDollarReads))(BankAccoutnModulusCheck.apply _)

  implicit val format: OFormat[BankAccoutnModulusCheck] = OFormat(reads, writes)
}
