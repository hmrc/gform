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

package uk.gov.hmrc.gform.form

import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

trait FormAlgebra[F[_]] {
  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form]

  def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): F[Form]

  def getAll(userId: UserId, formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): F[List[FormOverview]]

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit]

  def unstuck(formId: FormId)(implicit hc: HeaderCarrier): F[Unit]

  def create(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AffinityGroup],
    queryParams: QueryParams
  )(implicit hc: HeaderCarrier): F[FormIdData]

  def createFormFromLegacy(formIdData: FormIdData, newFormIdData: FormIdData)(implicit hc: HeaderCarrier): F[Form]

  def updateUserData(formIdData: FormIdData, userData: UserData)(implicit hc: HeaderCarrier): F[Unit]

  def changeVersion(formIdData: FormIdData, formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): F[Form]

  def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus]

  def forceUpdateFormStatus(formIdData: FormIdData, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit]

  def forceUpdateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[Unit]

  def getFormByEnvelopeId(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[Form]
}
