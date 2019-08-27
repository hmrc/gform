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

package uk.gov.hmrc.gform.form

import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

trait FormAlgebra[F[_]] {
  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form]

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit]

  def create(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AffinityGroup],
    expiryDays: Long,
    initialFields: Seq[FormField])(implicit hc: HeaderCarrier): F[NewFormData]

  def updateUserData(formId: FormId, userData: UserData)(implicit hc: HeaderCarrier): F[Unit]

  def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus]

}
