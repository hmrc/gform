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

package uk.gov.hmrc.gform.formtemplate

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.core.fromFutureA
import uk.gov.hmrc.gform.formredirect.FormRedirect
import uk.gov.hmrc.gform.formredirect.FormRedirectService
import uk.gov.hmrc.gform.gformfrontend.GformFrontendModule
import uk.gov.hmrc.gform.handlebarstemplate.{ HandlebarsSchemaAlgebra, HandlebarsTemplateAlgebra }
import uk.gov.hmrc.gform.history.HistoryModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.notificationbanner.NotificationBannerModule
import uk.gov.hmrc.gform.repo.DeleteResult
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.{ HandlebarsSchema, HandlebarsSchemaId, HandlebarsTemplate, HandlebarsTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateRaw
import uk.gov.hmrc.gform.shutter.ShutterModule

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class FormTemplateModule(
  controllerComponents: ControllerComponents,
  mongoModule: MongoModule,
  shutterModule: ShutterModule,
  notificationBannerModule: NotificationBannerModule,
  handlebarsTemplateService: HandlebarsTemplateAlgebra[Future],
  handlebarsSchemaService: HandlebarsSchemaAlgebra[Future],
  historyModule: HistoryModule,
  configModule: ConfigModule,
  gformFrontendModule: GformFrontendModule
)(implicit
  ex: ExecutionContext
) {

  private val formTemplateRepo: Repo[FormTemplate] =
    new Repo[FormTemplate]("formTemplate", mongoModule.mongoComponent, _._id.value)
  private val formTemplateRawRepo: Repo[FormTemplateRaw] =
    new Repo[FormTemplateRaw]("formTemplateRaw", mongoModule.mongoComponent, _._id.value)
  private val formRedirectRepo: Repo[FormRedirect] =
    new Repo[FormRedirect]("formRedirect", mongoModule.mongoComponent, _._id.value)

  val foptHandlebarsPayloadService: HandlebarsTemplateAlgebra[FOpt] = new HandlebarsTemplateAlgebra[FOpt] {
    override def save(handlebarsTemplate: HandlebarsTemplate): FOpt[Unit] =
      fromFutureA(handlebarsTemplateService.save(handlebarsTemplate))

    override def get(handlebarsTemplateId: HandlebarsTemplateId): FOpt[Option[HandlebarsTemplate]] =
      fromFutureA(handlebarsTemplateService.get(handlebarsTemplateId))

    override def delete(handlebarsTemplateId: HandlebarsTemplateId): FOpt[DeleteResult] =
      fromFutureA(handlebarsTemplateService.delete(handlebarsTemplateId))

    override def getAll: FOpt[List[HandlebarsTemplateId]] =
      fromFutureA(handlebarsTemplateService.getAll)
  }

  val foptHandlebarsSchemaService: HandlebarsSchemaAlgebra[FOpt] = new HandlebarsSchemaAlgebra[FOpt] {
    override def save(handlebarsSchema: HandlebarsSchema): FOpt[Unit] =
      fromFutureA(handlebarsSchemaService.save(handlebarsSchema))

    override def get(handlebarsSchemaId: HandlebarsSchemaId): FOpt[Option[HandlebarsSchema]] =
      fromFutureA(handlebarsSchemaService.get(handlebarsSchemaId))

    override def delete(handlebarsSchemaId: HandlebarsSchemaId): FOpt[DeleteResult] =
      fromFutureA(handlebarsSchemaService.delete(handlebarsSchemaId))

    override def getAllIds: FOpt[List[HandlebarsSchemaId]] =
      fromFutureA(handlebarsSchemaService.getAllIds)
  }

  val formTemplateService: FormTemplateService =
    new FormTemplateService(
      formTemplateRepo,
      formTemplateRawRepo,
      formRedirectRepo,
      foptHandlebarsPayloadService,
      foptHandlebarsSchemaService,
      configModule.appConfig,
      gformFrontendModule.gformFrontendConnector
    )
  val formRedirectService: FormRedirectService =
    new FormRedirectService(formRedirectRepo)

  val handler = new FormTemplatesControllerRequestHandler(
    formTemplateService.verifyAndSave,
    formTemplateService.save,
    historyModule.historyService.save,
    gformFrontendModule.gformFrontendConnector.saveFormTemplateCache
  ).futureInterpreter

  val formTemplatesController: FormTemplatesController =
    new FormTemplatesController(
      controllerComponents,
      formTemplateService,
      formRedirectService,
      shutterModule.shutterService,
      notificationBannerModule.notificationService,
      handler
    )

  val fOptFormTemplateAlgebra: FormTemplateAlgebra[FOpt] = new FormTemplateAlgebra[FOpt] {
    override def get(id: FormTemplateId): FOpt[FormTemplate] = fromFutureA(formTemplateService.get(id))

    override def find(id: FormTemplateId): FOpt[Option[FormTemplate]] = fromFutureA(formTemplateService.find(id))
  }
}
