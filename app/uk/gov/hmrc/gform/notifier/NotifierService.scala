/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.notifier

import cats.ApplicativeError
import cats.syntax.show._
import org.slf4j.LoggerFactory
import uk.gov.service.notify.NotificationClientApi

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

class NotifierService[F[_]](client: NotificationClientApi)(implicit F: ApplicativeError[F, String])
    extends NotifierAlgebra[F] {
  private val logger = LoggerFactory.getLogger(getClass)

  override def email(details: NotifierEmail): F[Unit] =
    Try {
      client
        .sendEmail(details.templateId.value, details.to.value, details.personalisation.asJava, details.reference.value)
    } match {
      case Success(_) => F.pure(())
      case Failure(t) =>
        logger.error(show"Failed to send a Notifier email with template ID ${details.templateId}", t)
        F.raiseError(t.getMessage)
    }
}
