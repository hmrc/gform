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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ Format, JsError, JsObject, JsResult, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId

case class DestinationResult(
  destinationId: DestinationId,
  includeIf: Option[Boolean],
  customerId: Option[String],
  taxpayerId: Option[String],
  paymentReference: Option[String],
  nino: Option[String],
  utr: Option[String],
  postalCode: Option[String],
  pegaCaseId: Option[String],
  bankAccountName: Option[String],
  sortCode: Option[String],
  accountNumber: Option[String],
  rollNumber: Option[String],
  refundClaimReference: Option[String],
  data: Option[JsObject]
)

object DestinationResult {
  implicit val format: Format[DestinationResult] = Json.format[DestinationResult]
}

case class DestinationEvaluation(evaluation: List[DestinationResult])

object DestinationEvaluation {
  val empty = DestinationEvaluation(List.empty[DestinationResult])
  implicit val format: OFormat[DestinationEvaluation] = Json.format[DestinationEvaluation]
}

case class NRSOrchestratorDestinationResultData(
  saUtr: Option[String],
  ctUtr: Option[String],
  submissionReferenceId: Option[String]
)

case class NRSOrchestratorDestinationResult(
  id: DestinationId,
  includeIf: Option[Boolean],
  data: NRSOrchestratorDestinationResultData
)

object NRSOrchestratorDestinationResult {
  implicit val dataFormat: Format[NRSOrchestratorDestinationResultData] = Json.format
  def fromDestinationResult(destinationResult: DestinationResult): JsResult[NRSOrchestratorDestinationResult] =
    destinationResult.data
      .map { dataJson =>
        val data = dataJson.validate[NRSOrchestratorDestinationResultData]
        data.map { data =>
          NRSOrchestratorDestinationResult(
            destinationResult.destinationId,
            destinationResult.includeIf,
            data
          )
        }
      }
      .getOrElse(JsError("destination result has no data object"))
}
