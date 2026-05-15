/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.hip

import org.slf4j.LoggerFactory
import play.api.libs.json._
import uk.gov.hmrc.domain.EmpRef
import uk.gov.hmrc.gform.hip.models.NIEmployments
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

trait HipServiceAlgebra[F[_]] {
  def validateNIClaimReference(nino: String, claimReference: String, correlationId: CorrelationId): F[JsValue]
  def getEmployments(nino: String, taxYear: Int, correlationId: CorrelationId)(implicit
    ec: ExecutionContext
  ): F[JsValue]
}

class HipService(
  connector: HipAlgebra[Future]
) extends HipServiceAlgebra[Future] {

  private val logger = LoggerFactory.getLogger(getClass)

  def validateNIClaimReference(nino: String, claimReference: String, correlationId: CorrelationId): Future[JsValue] =
    connector.validateNIClaimReference(nino, claimReference, correlationId)

  def getEmployments(nino: String, taxYear: Int, correlationId: CorrelationId)(implicit
    ec: ExecutionContext
  ): Future[JsValue] =
    connector
      .getEmployments(nino, taxYear, correlationId)
      .map(transformNiEmployments)

  /** Transforms to the deprecated API response pattern for backwards compatibility
    */
  private def transformNiEmployments(result: JsValue): JsValue = {
    val transformed: List[JsObject] = result.validate[NIEmployments] match {
      case JsSuccess(niEmployments, _) =>
        niEmployments.individualsEmploymentDetails.map { employment =>
          val (payeNumber, taxDistrictNumber) =
            Try(EmpRef.fromIdentifiers(employment.employerReference.getOrElse(""))) match {
              case Failure(_)   => (None, None)
              case Success(ref) => (Some(ref.taxOfficeReference), Some(ref.taxOfficeNumber))
            }

          Json.obj(
            "employerName"      -> employment.payeSchemeOperatorName.fold[JsValue](JsNull)(s => JsString(s)),
            "sequenceNumber"    -> JsNumber(employment.employmentSequenceNumber),
            "worksNumber"       -> employment.worksNumber.fold[JsValue](JsNull)(s => JsString(s)),
            "taxDistrictNumber" -> taxDistrictNumber.fold[JsValue](JsNull)(s => JsString(s)),
            "payeNumber"        -> payeNumber.fold[JsValue](JsNull)(s => JsString(s)),
            "director"          -> employment.directorIdentifier.fold[JsValue](JsNull)(b => JsBoolean(b))
          )
        }
      case JsError(err) =>
        logger.error(s"Unable to transform employments data; $err")
        // If the HIP employments API returns 200 but transformation fails - return empty
        List.empty[JsObject]
    }

    Json.toJson(transformed)
  }
}
