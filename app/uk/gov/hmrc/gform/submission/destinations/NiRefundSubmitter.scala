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

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA, fromOptA }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.hip.HipAlgebra
import uk.gov.hmrc.gform.sharedmodel.DestinationResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }

import scala.concurrent.{ ExecutionContext, Future }

class NiRefundSubmitter(
  hipConnectorAlgebra: HipAlgebra[Future]
)(implicit ec: ExecutionContext)
    extends NiRefundSubmitterAlgebra[FOpt] {

  private val logger = LoggerFactory.getLogger(getClass)

  override def submitBankDetails(
    d: Destination.NiRefundClaimApi,
    maybeDesRes: Option[DestinationResult],
    submissionInfo: DestinationSubmissionInfo
  ): FOpt[Unit] = {
    val formId = submissionInfo.formId.value

    for {
      destinationResult <- validateDestinationResult(maybeDesRes, d.id, formId)
      bankDetails       <- extractBankDetails(destinationResult, d.id, formId)
      _                 <- submitToHip(bankDetails, submissionInfo.submission.envelopeId.value)
    } yield ()
  }

  private def validateDestinationResult(
    maybeDesRes: Option[DestinationResult],
    destinationId: DestinationId,
    formId: String
  ): FOpt[DestinationResult] =
    maybeDesRes match {
      case Some(dr) => fromOptA(Right(dr))
      case None =>
        val errorMsg = s"Destination result missing for destination ID: $destinationId, Form ID: $formId"
        logger.error(errorMsg)
        fromOptA(Left(UnexpectedState(errorMsg)))
    }

  private def extractBankDetails(
    destinationResult: DestinationResult,
    destinationId: DestinationId,
    formId: String
  ): FOpt[ClaimDetailsData] = {
    val maybeDetails = for {
      nino                 <- destinationResult.nino
      accountNumber        <- destinationResult.accountNumber
      sortCode             <- destinationResult.sortCode
      bankAccountName      <- destinationResult.bankAccountName
      refundClaimReference <- destinationResult.refundClaimReference
    } yield ClaimDetailsData(
      nino,
      bankAccountName,
      sortCode,
      accountNumber,
      destinationResult.rollNumber,
      refundClaimReference
    )

    maybeDetails match {
      case Some(details) => fromOptA(Right(details))
      case None =>
        val errorMsg = buildMissingParametersError(destinationResult, destinationId, formId)
        logger.error(errorMsg)
        fromOptA(Left(UnexpectedState(errorMsg)))
    }
  }

  private def submitToHip(claimDetails: ClaimDetailsData, correlationId: String): FOpt[Unit] =
    fromFutureA(
      hipConnectorAlgebra.niClaimUpdateBankDetails(
        claimDetails.nino,
        claimDetails.bankAccountName,
        claimDetails.sortCode,
        claimDetails.accountNumber,
        claimDetails.rollNumber,
        claimDetails.refundClaimReference,
        correlationId
      )
    ).map(_ => ())

  private def buildMissingParametersError(
    dr: DestinationResult,
    destinationId: DestinationId,
    formId: String
  ): String = {
    val missingFields = List(
      if (dr.nino.isEmpty) Some("nino") else None,
      if (dr.accountNumber.isEmpty) Some("accountNumber") else None,
      if (dr.sortCode.isEmpty) Some("sortCode") else None,
      if (dr.bankAccountName.isEmpty) Some("bankAccountName") else None,
      if (dr.refundClaimReference.isEmpty) Some("refundClaimReference") else None
    ).flatten

    s"Required parameters missing [${missingFields.mkString(", ")}] for destination ID: $destinationId, Form ID: $formId"
  }

  private case class ClaimDetailsData(
    nino: String,
    bankAccountName: String,
    sortCode: String,
    accountNumber: String,
    rollNumber: Option[String],
    refundClaimReference: String
  )
}
