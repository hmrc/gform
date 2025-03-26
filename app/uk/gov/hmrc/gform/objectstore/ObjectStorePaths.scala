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

package uk.gov.hmrc.gform.objectstore

import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.objectstore.client.Path

trait ObjectStorePaths {
  def permanent: Path.Directory
  def ephemeral: Path.Directory // Deleted on FileProcessed notification from SDES
}
object ObjectStorePaths {
  def dmsPaths(envelopeId: EnvelopeId) = new ObjectStorePaths {
    val permanent: Path.Directory = Path.Directory("envelopes/" + envelopeId.value)
    val ephemeral: Path.Directory = Path.Directory("sdes")
  }

  def hmrcIlluminatePaths(envelopeId: EnvelopeId) = new ObjectStorePaths {
    val permanent: Path.Directory = Path.Directory("hmrc-illuminate/envelopes/" + envelopeId.value)
    val ephemeral: Path.Directory = Path.Directory("sdes/hmrc-illuminate")
  }

  def dataStorePaths(envelopeId: EnvelopeId) = new ObjectStorePaths {
    val permanent: Path.Directory = Path.Directory("data-store/envelopes/" + envelopeId.value)
    val ephemeral: Path.Directory = Path.Directory("sdes/data-store")
  }
}
