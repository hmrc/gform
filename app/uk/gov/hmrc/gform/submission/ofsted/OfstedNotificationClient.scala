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

package uk.gov.hmrc.gform.submission.ofsted

import cats.{ Monad, MonadError }
import uk.gov.hmrc.gform.config.OfstedNotificationConf
import uk.gov.service.notify.{ SendEmailResponse, SendSmsResponse }
import cats.implicits._

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }

trait Notifier[F[_]] extends OfstedNotificationConf {

  def notifyBySms(templateId: String, phoneNumber: PhoneNumber, personalisation: Map[String, _])(
    implicit me: MonadError[F, String]): F[SendSmsResponse] =
    runNotification(Try(notificationClient.sendSms(templateId, phoneNumber.number, personalisation.asJava, "")))

  def notifyByEmail(templateId: String, emailAddress: EmailAddress, personalisation: Map[String, _])(
    implicit me: MonadError[F, String]): F[SendEmailResponse] =
    runNotification(Try(notificationClient.sendEmail(templateId, emailAddress.email, personalisation.asJava, "")))

  private def runNotification[T](fn: Try[T])(implicit me: MonadError[F, String]): F[T] = fn match {
    case Success(response) => me.pure(response)
    case Failure(ex)       => me.raiseError(s"Unable to notify reviewer ${ex.getMessage}")
  }
}

class OfstedNotificationClient[F[_]: Monad](notifier: Notifier[F]) {

  import notifier.ofstedNotification._

  def notify(personalisation: Map[String, Any])(
    implicit me: MonadError[F, String]): F[OfstedNotificationClientResponse] =
    for {
      smsResponse <- notifier.notifyBySms(template, PhoneNumber(phoneNumber), personalisation)
//      emailResponse <- notifier.notifyByEmail(template, EmailAddress(email), personalisation)
    } yield OfstedNotificationClientResponse(smsResponse, None)
}

//TODO remove Option once we have the template from Notify people
case class OfstedNotificationClientResponse(smsResponse: SendSmsResponse, emailResponse: Option[SendEmailResponse])
