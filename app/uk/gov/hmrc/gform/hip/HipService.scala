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

import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId

import scala.concurrent.Future

trait HipServiceAlgebra[F[_]] {
  def validateNIClaimReference(nino: String, claimReference: String, correlationId: CorrelationId): F[JsValue]
}

class HipService(
  connector: HipAlgebra[Future]
) extends HipServiceAlgebra[Future] {

  def validateNIClaimReference(nino: String, claimReference: String, correlationId: CorrelationId): Future[JsValue] =
    connector.validateNIClaimReference(nino, claimReference, correlationId)
}
