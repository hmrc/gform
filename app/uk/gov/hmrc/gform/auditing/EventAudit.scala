/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auditing

import cats.Monad
import uk.gov.hmrc.gform.wshttp.MicroserviceAuditConnector
import uk.gov.hmrc.play.audit.http.connector.AuditResult
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import io.monadless.stdlib.MonadlessFuture._

sealed trait Connector {
  val connector: DataEvent => Future[AuditResult] = MicroserviceAuditConnector.sendEvent
}


class EventAudit[F[_]: Monad] extends Connector {
  def runProgram(event: DataEvent): F[AuditResult] =
    implicitly[Monad[F]].pure(program(event))

  def program(event: DataEvent): AuditResult =
    unlift(connector(event))
}
