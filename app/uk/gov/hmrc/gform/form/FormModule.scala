/*
 * Copyright 2017 HM Revenue & Customs
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

import sun.misc.ObjectInputFilter.Config
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.des.DesConnector
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.save4later.{ Save4Later, Save4LaterModule }
import uk.gov.hmrc.gform.wshttp.{ WSHttp, WSHttpModule }

class FormModule(
    mongoModule: MongoModule,
    shortLivedCacheModule: Save4LaterModule,
    formTemplateModule: FormTemplateModule,
    fileUploadModule: FileUploadModule,
    wSHttpModule: WSHttpModule,
    configModule: ConfigModule
) {

  val save4later = new Save4Later(shortLivedCacheModule.shortLivedCache, scala.concurrent.ExecutionContext.Implicits.global)

  val formService = new FormService(save4later)

  val environment: String = configModule.typesafeConfig.getString("microservice.services.etmp-hod.environment")
  val auth: String = configModule.typesafeConfig.getString("microservice.services.etmp-hod.authorization-token")
  val isStub: Boolean = configModule.typesafeConfig.getBoolean("microservice.services.etmp-hod.stub")
  val desConnector: DesConnector = new DesConnector(wSHttpModule.auditableWSHttp, configModule.serviceConfig.baseUrl("etmp-hod"), environment, auth, isStub)

  val formController: FormController = new FormController(
    formTemplateModule.formTemplateService,
    fileUploadModule.fileUploadService,
    formService,
    desConnector
  )

}
