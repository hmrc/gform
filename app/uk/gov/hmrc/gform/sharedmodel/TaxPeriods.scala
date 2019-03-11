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

package uk.gov.hmrc.gform.sharedmodel

import java.util.Date

import julienrf.json.derived
import play.api.data.validation.ValidationError
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class ObligationDetail(
  status: String,
  inboundCorrespondenceFromDate: Date,
  inboundCorrespondenceToDate: Date,
  inboundCorrespondenceDueDate: Date,
  periodKey: String)

object ObligationDetail {
  implicit val format: OFormat[ObligationDetail] = Json.format[ObligationDetail]
}

case class ObligationDetails(obligationDetails: List[ObligationDetail])

object ObligationDetails {
  implicit val format: OFormat[ObligationDetails] = Json.format[ObligationDetails]
}

case class Obligation(obligations: List[ObligationDetails])

object Obligation {
  implicit val format: OFormat[Obligation] = Json.format[Obligation]
}

case class TaxResponse(id: HmrcTaxPeriod, obligation: Obligation)

object TaxResponse {
  implicit val format: OFormat[TaxResponse] = Json.format[TaxResponse]
}

case class TaxPeriodIdentifier(idType: IdType, idNumber: IdNumber, regimeType: RegimeType)

object TaxPeriodIdentifier {
  implicit val format: OFormat[TaxPeriodIdentifier] = Json.format[TaxPeriodIdentifier]
}

case class TaxPeriodInformation(
  hmrcTaxPeriod: HmrcTaxPeriod,
  idNumberValue: IdNumberValue,
  inboundCorrespondenceFromDate: Date,
  inboundCorrespondenceToDate: Date,
  periodKey: String)

object TaxPeriodInformation {
  implicit val format: OFormat[TaxPeriodInformation] = derived.oformat
}

sealed trait Obligations
final case object NotChecked extends Obligations
final case class RetrievedObligations(listOfObligations: List[TaxPeriodInformation]) extends Obligations

object Obligations {
  implicit val format: OFormat[Obligations] = derived.oformat[Obligations]
}

case class IdNumberValue(value: String) extends AnyVal

object IdNumberValue {
  implicit val format: OFormat[IdNumberValue] = derived.oformat
}

case class HmrcTaxPeriodWithEvaluatedId(hmrcTaxPeriod: HmrcTaxPeriod, idNumberValue: IdNumberValue)

object HmrcTaxPeriodWithEvaluatedId extends JsonUtils {
  implicit val format: OFormat[HmrcTaxPeriodWithEvaluatedId] = derived.oformat
}
