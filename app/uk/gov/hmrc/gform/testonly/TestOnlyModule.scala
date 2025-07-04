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

package uk.gov.hmrc.gform.testonly

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.des.DesConnector
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateAlgebra, FormTemplateModule }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.submission.SubmissionModule
import uk.gov.hmrc.gform.submission.destinations.DestinationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.{ ExecutionContext, Future }

class TestOnlyModule(
  mongoModule: MongoModule,
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule,
  playComponents: PlayComponents,
  formService: FormService[Future],
  testOnlyFormService: TestOnlyFormService,
  formTemplateModule: FormTemplateModule,
  destinationModule: DestinationModule,
  controllerComponents: ControllerComponents,
  submissionModule: SubmissionModule
)(implicit ex: ExecutionContext) {

  val enrolmentConnector =
    new EnrolmentConnector(
      wSHttpModule.httpClient,
      "http://enrolment-store-proxy.protected.mdtp:80/enrolment-store-proxy"
    )

  val desConnector = new DesConnector(
    wSHttpModule.httpClient,
    configModule.serviceConfig.baseUrl("etmp-hod"),
    configModule.desConfig
  )

  val formTemplateAlgebra: FormTemplateAlgebra[Future] = new FormTemplateAlgebra[Future] {
    override def get(id: FormTemplateId): Future[FormTemplate] = formTemplateModule.formTemplateService.get(id)

    override def find(id: FormTemplateId): Future[Option[FormTemplate]] =
      formTemplateModule.formTemplateService.find(id)
  }

  val testOnlyController: TestOnlyController =
    new TestOnlyController(
      configModule.controllerComponents,
      mongoModule.mongoComponent,
      enrolmentConnector,
      formService,
      formTemplateAlgebra,
      destinationModule.futureDestinationsProcessorModelService,
      submissionModule.dataStoreSubmitter,
      desConnector,
      testOnlyFormService,
      formTemplateModule.handler
    )
  val proxyActions = new Proxy(playComponents.wsClient, controllerComponents)
  val fUInterceptor: FUInterceptorController =
    new FUInterceptorController(
      configModule.controllerComponents,
      wSHttpModule.httpClient,
      configModule.serviceConfig,
      proxyActions
    )
}
