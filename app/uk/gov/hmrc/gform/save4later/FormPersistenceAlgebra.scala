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

package uk.gov.hmrc.gform.save4later

import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormId }
import uk.gov.hmrc.http.HeaderCarrier

trait FormPersistenceAlgebra[F[_]] {
  def find(formId: FormId)(implicit hc: HeaderCarrier): F[Option[Form]]
  def get(formId: FormId)(implicit hc: HeaderCarrier): F[Form]
  def upsert(formId: FormId, form: Form)(implicit hc: HeaderCarrier): F[Unit]
  def delete(formId: FormId)(implicit hc: HeaderCarrier): F[Unit]
}
