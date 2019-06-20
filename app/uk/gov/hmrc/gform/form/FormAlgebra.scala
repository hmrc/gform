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
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

trait FormAlgebra[F[_]] {
  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form]

  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit]

  def create(
    userId: UserId,
    formTemplateId: FormTemplateId,
    accessCode: Option[AccessCode],
    expiryDays: Long,
    initialFields: Seq[FormField])(implicit hc: HeaderCarrier): F[FormId]

  def updateUserData(formId: FormId, userData: UserData)(implicit hc: HeaderCarrier): F[Unit]

  def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): F[FormStatus]

  def updateDestinationSubmissionInfo(formId: FormId, info: Option[DestinationSubmissionInfo])(
    implicit hc: HeaderCarrier): F[Unit]

  def saveKeyStore(formId: FormId, data: Map[String, JsValue])(implicit hc: HeaderCarrier): F[Unit]

  def getKeyStore(formId: FormId)(implicit hc: HeaderCarrier): F[Option[Map[String, JsValue]]]
}
