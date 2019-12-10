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

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.gform.sharedmodel.SmartString

sealed trait Validator {
  def errorMessage: SmartString
}

case object Validator {
  private val templateReads: Reads[Validator] = Reads { json =>
    (json \ "validatorName").as[String] match {
      case "hmrcRosmRegistrationCheck" => json.validate[HmrcRosmRegistrationCheckValidator]
      case "bankAccountModulusCheck"   => json.validate[BankAccoutnModulusCheck]
      case unsupported                 => JsError("Unsupported '" + unsupported + "' kind of validator.")
    }
  }
  implicit val format: OFormat[Validator] = OFormatWithTemplateReadFallback(templateReads)
}

case class HmrcRosmRegistrationCheckValidator(
  errorMessage: SmartString,
  regime: String,
  utr: FormCtx,
  postcode: FormCtx)
    extends Validator

object HmrcRosmRegistrationCheckValidator {
  private val readCustom: Reads[HmrcRosmRegistrationCheckValidator] =
    ((JsPath \ "errorMessage").read[SmartString] and
      (JsPath \ "parameters" \ "regime").read[String] and
      (JsPath \ "parameters" \ "utr").read(FormCtx.readsForTemplateJson) and
      (JsPath \ "parameters" \ "postcode")
        .read(FormCtx.readsForTemplateJson))(HmrcRosmRegistrationCheckValidator.apply _)

  implicit val format: OFormat[HmrcRosmRegistrationCheckValidator] = OFormatWithTemplateReadFallback(readCustom)
}

case class BankAccoutnModulusCheck(errorMessage: SmartString, accountNumber: FormCtx, sortCode: FormCtx)
    extends Validator

object BankAccoutnModulusCheck {
  private val readCustom: Reads[BankAccoutnModulusCheck] =
    ((JsPath \ "errorMessage").read[SmartString] and
      (JsPath \ "parameters" \ "accountNumber").read(FormCtx.readsForTemplateJson) and
      (JsPath \ "parameters" \ "sortCode").read(FormCtx.readsForTemplateJson))(BankAccoutnModulusCheck.apply _)

  implicit val format: OFormat[BankAccoutnModulusCheck] = OFormatWithTemplateReadFallback(readCustom)
}
