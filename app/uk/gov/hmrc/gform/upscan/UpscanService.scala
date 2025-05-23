/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.upscan

import java.time.Instant
import scala.concurrent.Future

class UpscanService(
  upscanRepository: UpscanRepository
) extends UpscanAlgebra[Future] {
  def confirm(upscanCallbackSuccess: UpscanCallback.Success): Future[UpscanConfirmation] =
    upscanRepository.upsert(
      UpscanConfirmation(
        upscanCallbackSuccess.reference,
        upscanCallbackSuccess.fileStatus,
        ConfirmationFailure.AllOk,
        Instant.now(),
        Some(upscanCallbackSuccess.uploadDetails.fileName)
      )
    )

  def reject(
    reference: UpscanReference,
    fileStatus: UpscanFileStatus,
    confirmationFailure: ConfirmationFailure
  ): Future[UpscanConfirmation] =
    upscanRepository.upsert(
      UpscanConfirmation(
        reference,
        fileStatus,
        confirmationFailure,
        Instant.now(),
        None
      )
    )

  def reference(upscanReference: UpscanReference): Future[Option[UpscanConfirmation]] =
    upscanRepository.find(upscanReference)

  def deleteReference(upscanReference: UpscanReference): Future[Unit] =
    upscanRepository.delete(upscanReference)

}
