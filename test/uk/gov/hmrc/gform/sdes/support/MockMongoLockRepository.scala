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

package uk.gov.hmrc.gform.sdes.support

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ times, verify, when }
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.mongo.lock.{ Lock, MongoLockRepository }

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.Future
import scala.concurrent.duration.Duration

trait MockMongoLockRepository extends MockitoSugar {
  protected val mockMongoLockRepository: MongoLockRepository = mock[MongoLockRepository]
  protected val lock: Lock = Lock("lockId", "ownerId", Instant.now(), Instant.now().plus(60, ChronoUnit.SECONDS))

  when(mockMongoLockRepository.takeLock(any[String], any[String], any[Duration]))
    .thenReturn(Future.successful(Some(lock)))
  when(mockMongoLockRepository.releaseLock(any[String], any[String]))
    .thenReturn(Future.successful(()))

  def verifyLockRepoInteractions(): Unit = {
    verify(mockMongoLockRepository, times(1)).takeLock(any[String], any[String], any[Duration])
    verify(mockMongoLockRepository, times(1)).releaseLock(any[String], any[String])
    ()
  }
}
