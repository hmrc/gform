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

package uk.gov.hmrc.gform.testonly

import scala.concurrent.Future
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.form.FormService
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateAlgebra, FormTemplateService }
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.playcomponents.PlayComponents
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.ExecutionContext

class TestOnlyModule(
  mongoModule: MongoModule,
  wSHttpModule: WSHttpModule,
  configModule: ConfigModule,
  playComponents: PlayComponents,
  formService: FormService[Future],
  formTemplateService: FormTemplateService)(implicit ex: ExecutionContext) {

  val enrolmentConnector =
    new EnrolmentConnector(
      wSHttpModule.auditableWSHttp,
      "http://enrolment-store-proxy.protected.mdtp:80/enrolment-store-proxy")

  val formTemplateAlgebra: FormTemplateAlgebra[Future] = new FormTemplateAlgebra[Future] {
    override def get(id: FormTemplateId): Future[FormTemplate] = formTemplateService.get(id)
  }

  val testOnlyController: TestOnlyController =
    new TestOnlyController(mongoModule.mongo, enrolmentConnector, formService, formTemplateAlgebra)
  val proxyActions = new Proxy(playComponents.ahcWSComponents.wsClient)
  val fUInterceptor: FUInterceptorController =
    new FUInterceptorController(wSHttpModule.auditableWSHttp, configModule.serviceConfig, proxyActions)
  val pdfGeneratorStub = new PdfGeneratorStubController
}
