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
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, FormIdData, FormOverview, FormStatus, QueryParams, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormModule(
  configModule: ConfigModule,
  objectStoreService: ObjectStoreAlgebra[Future],
  formService: FormService[Future]
)(implicit ex: ExecutionContext) {

  val formController: FormController =
    new FormController(
      configModule.controllerComponents,
      objectStoreService,
      formService
    )

  val fOptFormService: FormAlgebra[FOpt] = new FormAlgebra[FOpt] {
    override def get(formIdData: FormIdData)(implicit hc: HeaderCarrier): FOpt[Form] =
      fromFutureA(formService.get(formIdData))

    override def get(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Form] =
      fromFutureA(formService.get(formId))

    override def getAll(userId: UserId, formTemplateId: FormTemplateId)(implicit
      hc: HeaderCarrier
    ): FOpt[List[FormOverview]] =
      fromFutureA(formService.getAll(userId, formTemplateId))

    override def delete(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.delete(formId))

    override def unstuck(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.unstuck(formId))

    override def create(
      userId: UserId,
      formTemplateId: FormTemplateId,
      affinityGroup: Option[AffinityGroup],
      queryParams: QueryParams
    )(implicit hc: HeaderCarrier): FOpt[FormIdData] =
      fromFutureA(formService.create(userId, formTemplateId, affinityGroup, queryParams))

    override def updateUserData(formIdData: FormIdData, userData: UserData)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.updateUserData(formIdData, userData))

    override def changeVersion(formIdData: FormIdData, formTemplateId: FormTemplateId)(implicit
      hc: HeaderCarrier
    ): FOpt[Form] =
      fromFutureA(formService.changeVersion(formIdData, formTemplateId))

    def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): FOpt[FormStatus] =
      fromFutureA(formService.updateFormStatus(formId, newStatus))

    override def forceUpdateFormStatus(formIdData: FormIdData, newStatus: FormStatus)(implicit
      hc: HeaderCarrier
    ): FOpt[Unit] =
      forceUpdateFormStatus(formIdData.toFormId, newStatus)

    override def forceUpdateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.forceUpdateFormStatus(formId, newStatus))

    override def createFormFromLegacy(formIdData: FormIdData, newFormIdData: FormIdData)(implicit
      hc: HeaderCarrier
    ): FOpt[Form] =
      fromFutureA(formService.createFormFromLegacy(formIdData, newFormIdData))

    override def getFormByEnvelopeId(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): FOpt[Form] =
      fromFutureA(formService.getFormByEnvelopeId(envelopeId))
  }

}
