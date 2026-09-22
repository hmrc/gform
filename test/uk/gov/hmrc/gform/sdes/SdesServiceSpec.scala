/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sdes

import cats.data.EitherT
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.SystemMaterializer
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ times, verify, when }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.sdes.NotificationStatus.{ FileProcessed, FileReady, FileReceived }
import uk.gov.hmrc.gform.sharedmodel.sdes.{ CallBackNotification, CorrelationId, NotificationStatus, SdesDestination, SdesHistory, SdesNotifyRequest, SdesSubmission }
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.lock.{ Lock, MongoLockRepository }

import java.time.Instant
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.concurrent.{ Await, ExecutionContext, Future }

class SdesServiceSpec extends AnyFlatSpec with Matchers with MockitoSugar with ScalaFutures with BeforeAndAfterAll {

  implicit private val system: ActorSystem = ActorSystem("sdes-service-spec")
  implicit private val ec: ExecutionContext = system.dispatcher
  implicit private val materializer: org.apache.pekko.stream.Materializer = SystemMaterializer(system).materializer
  implicit private val hc: HeaderCarrier = HeaderCarrier()
  implicit private val scheduler: org.apache.pekko.actor.Scheduler = system.scheduler

  override protected def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
    super.afterAll()
  }

  "update" should "mark submission as received and NOT attempt to delete zip files when FileReceived message" in new Fixture {
    val results = Await.result(sdesService.update(fileReceivedNotification), 10.seconds)

    results shouldBe Right(())
    current.get.status shouldBe FileReceived
    current.get.isProcessed shouldBe false
    verify(mockSdesConnector, times(0)).getPublicKey()(hc)
    verify(mockSdesConnector, times(0)).notifySDES(any[SdesNotifyRequest], any[SdesRouting])(any[HeaderCarrier])
    verify(mockObjectStoreService, times(0))
      .deleteZipFile(any[EnvelopeId], any[uk.gov.hmrc.gform.objectstore.ObjectStorePaths])(any[HeaderCarrier])
  }

  it should "mark submission as processed and attempt to delete zip files when FileProcessed message" in new Fixture {
    val results = Await.result(sdesService.update(fileProcessedNotification), 10.seconds)

    results shouldBe Right(())
    current.get.status shouldBe FileProcessed
    current.get.isProcessed shouldBe true
    verify(mockSdesConnector, times(0)).getPublicKey()(hc)
    verify(mockSdesConnector, times(0)).notifySDES(any[SdesNotifyRequest], any[SdesRouting])(any[HeaderCarrier])
    verify(mockObjectStoreService, times(1))
      .deleteZipFile(any[EnvelopeId], any[uk.gov.hmrc.gform.objectstore.ObjectStorePaths])(any[HeaderCarrier])
  }

  it should "preserve the higher priority submission details when callbacks arrive concurrently IN order" in new Fixture {
    val results =
      Await.result(
        Future.sequence(
          List(sdesService.update(fileReceivedNotification), sdesService.update(fileProcessedNotification))
        ),
        10.seconds
      )

    results shouldBe List(Right(()), Right(()))
    current.get.status shouldBe FileProcessed
    current.get.isProcessed shouldBe true
    verify(mockSubmissionRepo, times(2)).find(correlationId)
    verify(mockSdesConnector, times(0)).getPublicKey()(hc)
    verify(mockSdesConnector, times(0)).notifySDES(any[SdesNotifyRequest], any[SdesRouting])(any[HeaderCarrier])
    verify(mockObjectStoreService, times(1))
      .deleteZipFile(any[EnvelopeId], any[uk.gov.hmrc.gform.objectstore.ObjectStorePaths])(any[HeaderCarrier])
  }

  it should "preserve the higher priority submission details when callbacks arrive concurrently OUT OF order" in new Fixture {
    val results =
      Await.result(
        Future.sequence(
          List(sdesService.update(fileProcessedNotification), sdesService.update(fileReceivedNotification))
        ),
        10.seconds
      )

    results shouldBe List(Right(()), Right(()))
    current.get.status shouldBe FileProcessed
    current.get.isProcessed shouldBe true
    verify(mockSubmissionRepo, times(2)).find(correlationId)
    verify(mockSdesConnector, times(0)).getPublicKey()(hc)
    verify(mockSdesConnector, times(0)).notifySDES(any[SdesNotifyRequest], any[SdesRouting])(any[HeaderCarrier])
    verify(mockObjectStoreService, times(1))
      .deleteZipFile(any[EnvelopeId], any[uk.gov.hmrc.gform.objectstore.ObjectStorePaths])(any[HeaderCarrier])
  }

  trait Fixture {
    val correlationId = "correlation-id"
    val initialStateSdesSubmission: SdesSubmission = submission(correlationId, FileReady)
    val current = new AtomicReference[SdesSubmission](initialStateSdesSubmission)
    val lockHeld = new AtomicBoolean(false)

    val mockSdesConnector: SdesConnector = mock[SdesConnector]
    val mockSubmissionRepo: Repo[SdesSubmission] = mock[Repo[SdesSubmission]]
    val mockLockRepository: MongoLockRepository = mock[MongoLockRepository]
    val mockDestinationWorkItemService: DestinationWorkItemAlgebra[Future] = mock[DestinationWorkItemAlgebra[Future]]
    val mockEnvelopeService: EnvelopeAlgebra[Future] = mock[EnvelopeAlgebra[Future]]
    val mockSdesHistoryService: SdesHistoryAlgebra[Future] = mock[SdesHistoryAlgebra[Future]]
    val mockObjectStoreService: ObjectStoreAlgebra[Future] = mock[ObjectStoreAlgebra[Future]]

    val fileProcessedNotification = CallBackNotification(FileProcessed, "file.xml", correlationId, None)
    val fileReceivedNotification = CallBackNotification(FileReceived, "file.xml", correlationId, None)

    when(mockSubmissionRepo.find(any[String])).thenAnswer((_: org.mockito.invocation.InvocationOnMock) =>
      Future.successful(Some(current.get))
    )
    when(mockSubmissionRepo.upsert(any[SdesSubmission])).thenAnswer {
      (invocation: org.mockito.invocation.InvocationOnMock) =>
        current.set(invocation.getArgument[SdesSubmission](0))
        EitherT.rightT[Future, UnexpectedState](())
    }
    when(mockSdesHistoryService.save(any[SdesHistory])).thenReturn(Future.unit)
    when(
      mockObjectStoreService
        .deleteZipFile(any[EnvelopeId], any[uk.gov.hmrc.gform.objectstore.ObjectStorePaths])(any[HeaderCarrier])
    )
      .thenReturn(Future.unit)

    when(mockLockRepository.takeLock(any[String], any[String], any[FiniteDuration]))
      .thenAnswer((_: org.mockito.invocation.InvocationOnMock) =>
        Future.successful(if (lockHeld.compareAndSet(false, true)) Some(lock) else None)
      )
    when(mockLockRepository.releaseLock(any[String], any[String])).thenAnswer {
      (_: org.mockito.invocation.InvocationOnMock) =>
        lockHeld.set(false)
        Future.unit
    }

    val sdesService = new SdesService(
      mockSdesConnector,
      mockSubmissionRepo,
      mockDestinationWorkItemService,
      mockEnvelopeService,
      config,
      mockSdesHistoryService,
      mockObjectStoreService,
      "http://localhost",
      mockLockRepository
    )
  }

  private def submission(correlationId: String, status: NotificationStatus): SdesSubmission =
    SdesSubmission(
      _id = CorrelationId(correlationId),
      envelopeId = EnvelopeId("envelope-id"),
      formTemplateId = FormTemplateId("form-template-id"),
      submissionRef = SubmissionRef("submission-ref"),
      status = status,
      destination = Some(SdesDestination.Dms)
    )

  private val lock: Lock = Lock("lock-id", "owner-id", Instant.now(), Instant.now().plusSeconds(2))

  private val routing = SdesRouting("client-id", "information-type", "recipient")
  private def config = SdesConfig(
    basePath = "/sdes",
    fileLocationUrl = "http://localhost",
    dms = routing,
    hmrcIlluminate = routing,
    dataStore = routing,
    lockTTL = 2000,
    welshDefaults = WelshDefaults("classification", "business-area"),
    infoArchive = routing,
    caseflow = routing,
    dataLakehouse = routing
  )
}
