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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.gform.nrs.NrsPayload
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.{ NRSOrchestratorDestinationResultData, SubmissionRef }

final case class NrsOrchestratorWorkItem(
  envelopeId: EnvelopeId,
  businessId: String,
  notableEvent: String,
  onSubmitHeaders: Seq[(String, String)],
  destinationResultData: NRSOrchestratorDestinationResultData,
  submissionRef: SubmissionRef,
  payload: NrsPayload,
  submissionDate: String,
  userAuthToken: String,
  identityData: JsObject
)

object NrsOrchestratorWorkItem {
  val format: OFormat[NrsOrchestratorWorkItem] = Json.format[NrsOrchestratorWorkItem]
  def formatEncrypted(implicit jsonCrypto: Encrypter with Decrypter): OFormat[NrsOrchestratorWorkItem] =
    JsonCryptoUtils.formatEncrypted(format)(jsonCrypto)
}
