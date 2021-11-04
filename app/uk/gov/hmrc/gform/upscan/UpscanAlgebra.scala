/*
 * Copyright 2021 HM Revenue & Customs
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

import akka.util.ByteString
import scala.language.higherKinds

trait UpscanAlgebra[F[_]] {

  def download(
    downloadUrl: String
  ): F[ByteString]

  def confirm(upscanCallbackSuccess: UpscanCallback.Success): F[UpscanConfirmation]
  def reject(upscanCallbackFailure: UpscanCallback.Failure): F[UpscanConfirmation]

  def reference(upscanReference: UpscanReference): F[Option[UpscanConfirmation]]
  def deleteReference(upscanReference: UpscanReference): F[Unit]
}
