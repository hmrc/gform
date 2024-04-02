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

package uk.gov.hmrc.gform.submission.destinations

import uk.gov.hmrc.gform.sdes.SdesRouting
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, UserSession }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.DataStore
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree

trait DataStoreSubmitterAlgebra[F[_]] {
  def submitPayload(
    submissionInfo: DestinationSubmissionInfo,
    payload: String,
    dataStoreRouting: SdesRouting,
    destination: SdesDestination
  ): F[Unit]

  def generatePayload(
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    dataStore: DataStore,
    l: LangADT,
    userSession: UserSession,
    taxpayerId: Option[String],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): String

  def validateSchema(
    dataStore: DataStore,
    payload: String
  ): Either[String, Unit]
}
