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

package uk.gov.hmrc.gform.sdes.renotify

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ never, times, verify, when }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import uk.gov.hmrc.gform.sdes.support.MockMongoLockRepository
import uk.gov.hmrc.gform.sdes.{ SdesAlgebra, SdesRenotifyService }
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileProcessingFailure, FileReady, FileReceived }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.Dms
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

class SdesRenotifyQScheduledServiceSpec extends AnyFlatSpec with MockMongoLockRepository {

  "invoke" should "renotify SDES for FileProcessingFailure statuses only" in new Fixture {
    when(
      mockSdesService.searchAll(
        any[Option[Boolean]],
        any[Option[String]],
        any[Option[NotificationStatus]],
        any[Option[SdesDestination]],
        any[Option[Int]]
      )
    ).thenReturn(Future.successful(SdesSubmissionPageData(List(submissionData), 1)))
    when(mockSdesRenotifyService.renotifySDES(CorrelationId(any[String]))(any[HeaderCarrier]))
      .thenReturn(Future.successful(HttpResponse(200, "")))

    Await.result(renotifyScheduledService.invoke, 10.seconds)

    verifyLockRepoInteractions()
    verify(mockSdesService, never()).searchAll(None, None, Some(FileReady), Some(Dms), showBeforeSubmittedAt)
    verify(mockSdesService, never()).searchAll(None, None, Some(FileReceived), Some(Dms), showBeforeSubmittedAt)
    verify(mockSdesService, times(1))
      .searchAll(None, None, Some(FileProcessingFailure), Some(Dms), showBeforeSubmittedAt)
    verify(mockSdesRenotifyService, times(1)).renotifySDES(CorrelationId(any[String]))(any[HeaderCarrier])
  }

  class Fixture {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val renotifyDestination: Seq[SdesDestination] = List("Dms").map(SdesDestination.fromString)
    val mongodbLockTimeoutDuration: Duration = 1.minutes
    val showBeforeSubmittedAt: Option[Int] = Some(7)

    val mockSdesService: SdesAlgebra[Future] = mock[SdesAlgebra[Future]]
    val mockSdesRenotifyService: SdesRenotifyService = mock[SdesRenotifyService]

    val submissionData = SdesSubmissionData(
      correlationId = CorrelationId("correlation-id"),
      envelopeId = EnvelopeId("envelope-id"),
      formTemplateId = FormTemplateId("form-template-id"),
      submissionRef = SubmissionRef("submission-ref"),
      submittedAt = Some(Instant.now()),
      status = FileProcessingFailure,
      failureReason = "Some failure reason",
      lastUpdated = Some(Instant.now()),
      numberOfFiles = 1,
      uploadCount = 1,
      size = 3000,
      destination = Dms
    )

    val renotifyScheduledService = new SdesRenotifyQScheduledService(
      renotifyDestination,
      mockSdesRenotifyService,
      mockSdesService,
      mockMongoLockRepository,
      mongodbLockTimeoutDuration,
      showBeforeSubmittedAt
    )
  }
}
