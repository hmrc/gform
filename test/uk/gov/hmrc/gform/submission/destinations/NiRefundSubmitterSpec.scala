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

import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.hip.HipAlgebra
import uk.gov.hmrc.gform.sharedmodel.DestinationResult
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class NiRefundSubmitterSpec extends AnyWordSpecLike with Matchers with MockFactory with ScalaFutures {

  private val mockHipConnector = mock[HipAlgebra[Future]]
  private val submitter = new NiRefundSubmitter(mockHipConnector)

  private val destinationId = DestinationId("ni-refund-test")
  private val formId = FormId("test-form-123")
  private val customerId = "customer-123"

  private val niRefundClaimApiDestination = Destination.NiRefundClaimApi(
    id = destinationId,
    includeIf = HandlebarValue("true"),
    failOnError = true,
    bankAccountName = FormCtx(FormComponentId("bankAccountName")),
    sortCode = FormCtx(FormComponentId("sortCode")),
    accountNumber = FormCtx(FormComponentId("accountNumber")),
    rollNumber = None,
    refundClaimReference = FormCtx(FormComponentId("refundClaimReference")),
    nino = FormCtx(FormComponentId("nino"))
  )

  private def createSubmissionInfo(formId: FormId, customerId: String): DestinationSubmissionInfo = {
    val dmsMetaData = DmsMetaData(
      formTemplateId = FormTemplateId("test-template"),
      customerId = customerId
    )

    val submission = Submission(
      _id = SubmissionId(formId, EnvelopeId("ENV123")),
      submittedDate = LocalDateTime.now(),
      submissionRef = SubmissionRef("SUB123"),
      envelopeId = EnvelopeId("ENV123"),
      noOfAttachments = 0,
      dmsMetaData = dmsMetaData
    )

    DestinationSubmissionInfo(
      customerId = customerId,
      submission = submission
    )
  }

  private def createDestinationResult(
    nino: Option[String] = Some("AB123456C"),
    accountNumber: Option[String] = Some("12345678"),
    sortCode: Option[String] = Some("123456"),
    bankAccountName: Option[String] = Some("John Doe"),
    refundClaimReference: Option[String] = Some("REF123456"),
    rollNumber: Option[String] = None
  ): DestinationResult =
    DestinationResult(
      destinationId = destinationId,
      includeIf = Some(true),
      customerId = None,
      taxpayerId = None,
      paymentReference = None,
      nino = nino,
      utr = None,
      postalCode = None,
      pegaCaseId = None,
      bankAccountName = bankAccountName,
      sortCode = sortCode,
      accountNumber = accountNumber,
      rollNumber = rollNumber,
      refundClaimReference = refundClaimReference
    )

  private val successResponse: JsValue = Json.obj("status" -> "success")

  "NiRefundSubmitter.submitBankDetails" should {
    "successfully submit bank details when all required fields are present" in {
      val destinationResult = createDestinationResult()
      val submissionInfo = createSubmissionInfo(formId, customerId)

      (mockHipConnector.niClaimUpdateBankDetails _)
        .expects(
          "AB123456C",
          "John Doe",
          "123456",
          "12345678",
          None,
          "REF123456",
          "ENV123"
        )
        .returning(Future.successful(successResponse))
        .once()

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      whenReady(result.value) { either =>
        either shouldBe Right(())
      }
    }

    "successfully submit bank details with roll number when present" in {
      val destinationResult = createDestinationResult(rollNumber = Some("ROLL123"))
      val submissionInfo = createSubmissionInfo(formId, customerId)

      (mockHipConnector.niClaimUpdateBankDetails _)
        .expects(
          "AB123456C",
          "John Doe",
          "123456",
          "12345678",
          Some("ROLL123"),
          "REF123456",
          "ENV123"
        )
        .returning(Future.successful(successResponse))
        .once()

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      whenReady(result.value) { either =>
        either shouldBe Right(())
      }
    }

    "fail when destination result is missing" in {
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        None,
        submissionInfo
      )

      whenReady(result.value) { either =>
        either shouldBe Left(
          UnexpectedState(s"Destination result missing for destination ID: $destinationId, Form ID: ${formId.value}")
        )
      }
    }

    "fail when NINO is missing" in {
      val destinationResult = createDestinationResult(nino = None)
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("nino")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "fail when account number is missing" in {
      val destinationResult = createDestinationResult(accountNumber = None)
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("accountNumber")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "fail when sort code is missing" in {
      val destinationResult = createDestinationResult(sortCode = None)
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("sortCode")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "fail when bank account name is missing" in {
      val destinationResult = createDestinationResult(bankAccountName = None)
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("bankAccountName")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "fail when refund claim reference is missing" in {
      val destinationResult = createDestinationResult(refundClaimReference = None)
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("refundClaimReference")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "fail with all missing fields listed when multiple fields are missing" in {
      val destinationResult = createDestinationResult(
        nino = None,
        accountNumber = None,
        sortCode = None
      )
      val submissionInfo = createSubmissionInfo(formId, customerId)

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      result.value.map {
        case Left(error) =>
          error.error should include("nino")
          error.error should include("accountNumber")
          error.error should include("sortCode")
          error.error should include("Required parameters missing")
        case Right(_) =>
          fail("Expected a Left with error message, but got Right")
      }
    }

    "propagate HIP connector failures" in {
      val destinationResult = createDestinationResult()
      val submissionInfo = createSubmissionInfo(formId, customerId)
      val expectedException = new RuntimeException("HIP service unavailable")

      (mockHipConnector.niClaimUpdateBankDetails _)
        .expects(*, *, *, *, *, *, *)
        .returning(Future.failed(expectedException))
        .once()

      val result = submitter.submitBankDetails(
        niRefundClaimApiDestination,
        Some(destinationResult),
        submissionInfo
      )

      whenReady(result.value.failed) { throwable =>
        throwable shouldBe expectedException
      }
    }

    "NiRefundSubmitter error messages" should {
      "include destination ID and form ID in error messages" in {
        val submissionInfo = createSubmissionInfo(formId, customerId)

        val result = submitter.submitBankDetails(
          niRefundClaimApiDestination,
          None,
          submissionInfo
        )

        result.value.map {
          case Left(error) =>
            error.error should include(destinationId.id)
            error.error should include(formId.value)
          case Right(_) =>
            fail("Expected a Left with error message, but got Right")
        }
      }

      "provide detailed information about which fields are missing" in {
        val destinationResult = createDestinationResult(
          nino = None,
          bankAccountName = None
        )
        val submissionInfo = createSubmissionInfo(formId, customerId)

        val result = submitter.submitBankDetails(
          niRefundClaimApiDestination,
          Some(destinationResult),
          submissionInfo
        )

        result.value.map {
          case Left(error) =>
            val errorMessage = error.error
            errorMessage should include("[")
            errorMessage should include("]")
            errorMessage should include("nino")
            errorMessage should include("bankAccountName")
          case Right(_) =>
            fail("Expected a Left with error message, but got Right")
        }
      }
    }
  }
}
