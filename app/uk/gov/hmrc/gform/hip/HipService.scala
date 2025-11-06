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
import uk.gov.hmrc.http.InternalServerException

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
          val payeNumber = Try(EmpRef.fromIdentifiers(employment.payeNumber)) match {
            case Failure(_)   => employment.payeNumber
            case Success(ref) => ref.taxOfficeReference
          }

          Json.obj(
            "employerName"      -> JsString(employment.payeSchemeOperatorName),
            "sequenceNumber"    -> JsNumber(employment.employmentSequenceNumber),
            "worksNumber"       -> JsString(employment.worksNumber),
            "taxDistrictNumber" -> JsString(employment.taxDistrictNumber),
            "payeNumber"        -> JsString(payeNumber),
            "director"          -> JsBoolean(employment.directorIdentifier)
          )
        }
      case JsError(err) =>
        val msg = s"Unable to transform employments data; $err"
        logger.error(msg)
        throw new InternalServerException(msg)
    }

    Json.toJson(transformed)
  }
}
