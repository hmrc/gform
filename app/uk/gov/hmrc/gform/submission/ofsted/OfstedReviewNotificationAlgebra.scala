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

import cats.MonadError
import cats.syntax.applicative._
import uk.gov.hmrc.gform.sharedmodel.form.FormId

trait OfstedReviewNotificationAlgebra[F[_]] {
  def requestReview(ofstedNotification: OfstedNotification): F[Unit]
  def reject(reviewedFormId: FormId, rejectionComment: String): F[Unit]
  def approve(reviewedFormId: FormId): F[Unit]
}

class OfstedEmailReviewNotifier[F[_]](notificationClient: OfstedNotificationClient[F])(
  implicit monadError: MonadError[F, String])
    extends OfstedReviewNotificationAlgebra[F] {
  override def requestReview(ofstedNotification: OfstedNotification): F[Unit] = {
    notificationClient.notify(personlise("no id yet", "Submitted"))
    println(s"Review form: ${ofstedNotification.formId}")
  }.pure

  override def reject(reviewedFormId: FormId, rejectionComment: String): F[Unit] = {
    notificationClient.notify(personlise(reviewedFormId.value, "Rejected"))
    println(s"Rejected: ${reviewedFormId.value} with comment: $rejectionComment")
  }.pure

  override def approve(reviewedFormId: FormId): F[Unit] = {
    notificationClient.notify(personlise(reviewedFormId.value, "Approved"))
    println(s"Approved: ${reviewedFormId.value}")
  }.pure

  private val personlise: (String, String) => Map[String, Any] =
    (id, status) => Map[String, Any]("formId" -> id, "status" -> status)
}
