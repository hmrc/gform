/*
 * Copyright 2022 HM Revenue & Customs
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

import java.time.LocalDateTime

import cats.Id
import cats.implicits._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.FileUploadAlgebra
import uk.gov.hmrc.gform.formmetadata.FormMetadataAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.save4later.FormPersistenceAlgebra
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormIdData, QueryParams }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

class FormServiceSpec extends Spec {

  it should "create and persist a form" in {
    val formTemplateId = formTemplate._id
    val formIdData = FormIdData.Plain(UserId("usr"), FormTemplateId("AAA999"))
    val persistenceAlgebra = mock[FormPersistenceAlgebra[Id]]
    val fileUploadAlgebra = mock[FileUploadAlgebra[Id]]
    val formTemplateAlgebra = mock[FormTemplateAlgebra[Id]]
    val metadataAlgebra = mock[FormMetadataAlgebra[Id]]

    (persistenceAlgebra
      .upsert(_: Form)(_: HeaderCarrier))
      .expects(*, *)
      .returning(().pure[Id])

    (metadataAlgebra
      .upsert(_: FormIdData))
      .expects(formIdData.lowerCaseId)
      .returning(().pure[Id])

    (fileUploadAlgebra
      .createEnvelope(_: FormTemplateId, _: LocalDateTime)(_: HeaderCarrier))
      .expects(formTemplateId, *, *)
      .returning(EnvelopeId("ev").pure[Id])

    (formTemplateAlgebra
      .get(_: FormTemplateId))
      .expects(formTemplateId)
      .returning(formTemplate.pure[Id])

    val service = new FormService[Id](persistenceAlgebra, fileUploadAlgebra, formTemplateAlgebra, metadataAlgebra)

    service.create(UserId("usr"), formTemplateId, None, 2L, QueryParams.empty)(
      HeaderCarrier()
    ) shouldBe formIdData.lowerCaseId
  }
}
