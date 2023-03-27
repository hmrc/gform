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

package uk.gov.hmrc.gform.formtemplate

import play.api.libs.json.{ JsDefined, JsError, JsString, JsSuccess, JsUndefined, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Email, EmailVerifiedBy, FormComponentId, TextConstraint }
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService

import scala.annotation.nowarn

sealed trait EmailVerification extends Product with Serializable {
  val textConstraint: TextConstraint = this match {
    case EmailVerification.NoVerification                         => Email
    case EmailVerification.VerifiedBy(fcId, emailVerifierService) => EmailVerifiedBy(fcId, emailVerifierService)
  }
}
object EmailVerification {

  val noVerification: EmailVerification = NoVerification
  def verifiedBy(formComponentId: FormComponentId, emailVerifierService: EmailVerifierService) =
    VerifiedBy(formComponentId, emailVerifierService)

  case object NoVerification extends EmailVerification
  case class VerifiedBy(formComponentId: FormComponentId, emailVerifierService: EmailVerifierService)
      extends EmailVerification
  @nowarn
  implicit val reads: Reads[EmailVerification] = Reads { json =>
    EmailVerifierService.format.reads(json).flatMap { emailVerifierService =>
      (json \ "codeField") match {
        case JsDefined(JsString(field)) => JsSuccess(verifiedBy(FormComponentId(field), emailVerifierService))
        case JsDefined(unknown)         => JsError(s"Expected string for field 'codeField', got $unknown")
        case JsUndefined()              => JsError(s"Missing field 'codeField' in json $json")
      }
    }
  }
}
