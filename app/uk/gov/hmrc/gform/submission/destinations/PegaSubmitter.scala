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

package uk.gov.hmrc.gform.submission.destinations

import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.hip.HipAlgebra
import uk.gov.hmrc.gform.sharedmodel.DestinationResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination

import scala.concurrent.{ ExecutionContext, Future }

class PegaSubmitter(
  hipConnectorAlgebra: HipAlgebra[Future]
)(implicit ec: ExecutionContext)
    extends PegaSubmitterAlgebra[FOpt] {

  override def getAndUpdateCase(
    d: Destination.PegaApi,
    maybeDesRes: Option[DestinationResult],
    submissionInfo: DestinationSubmissionInfo
  ): FOpt[Unit] = {
    val formId = submissionInfo.formId.value
    maybeDesRes match {
      case Some(dr) =>
        dr.pegaCaseId match {
          case Some(caseId) =>
            for {
              eTag <- fromFutureA(hipConnectorAlgebra.getPegaCaseActionDetails(caseId, "pyChangeStage"))
              _    <- fromFutureA(hipConnectorAlgebra.pegaChangeToNextStage(caseId, eTag))
            } yield ()
          case None =>
            throw new IllegalArgumentException(
              s"Pega case ID not evaluated for destination ID: ${d.id}, Form ID: $formId"
            )
        }
      case None =>
        throw new IllegalArgumentException(s"Destination result missing for destination ID: ${d.id}, Form ID: $formId")
    }
  }
}
