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

package uk.gov.hmrc.gform.submission

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.submission.destinations.{ DestinationModule, DestinationSubmitter, DestinationsSubmitter, DestinationsSubmitterAlgebra, DmsSubmitter }

import scala.concurrent.ExecutionContext

class SubmissionModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  pdfGeneratorModule: PdfGeneratorModule,
  formModule: FormModule,
  formTemplateModule: FormTemplateModule,
  fileUploadModule: FileUploadModule,
  wsHttpModule: WSHttpModule,
  timeModule: TimeModule,
  emailModule: EmailModule,
  handlebarsHttpApiModule: HandlebarsHttpApiModule,
  destinationModule: DestinationModule)(implicit ex: ExecutionContext) {

  //TODO: this should be replaced with save4later for submissions

  val submissionRepo = new SubmissionRepo(mongoModule.mongo)

  private val fileUploadServiceDmsSubmitter = new DmsSubmitter(
    fileUploadModule.fileUploadService,
    formModule.fOptFormService,
    formTemplateModule.fOptFormTemplateAlgebra,
    pdfGeneratorModule.pdfGeneratorService
  )

  private val realDestinationSubmitter = new DestinationSubmitter(
    fileUploadServiceDmsSubmitter,
    handlebarsHttpApiModule.handlebarsHttpSubmitter,
    destinationModule.destinationAuditer,
    formModule.fOptFormService
  )

  private val destinationsSubmitter: DestinationsSubmitterAlgebra[FOpt] = new DestinationsSubmitter(
    realDestinationSubmitter)

  val submissionService = new SubmissionService(
    formModule.fOptFormService,
    formTemplateModule.formTemplateService,
    destinationModule.destinationsProcessorModelService,
    destinationsSubmitter,
    submissionRepo,
    emailModule.emailLogic,
    timeModule.timeProvider
  )

  val submissionController = new SubmissionController(submissionService)

  val formBundleSubmissionService: Option[FormBundleSubmissionService[FOpt]] = for {
    formTreeService <- destinationModule.formTreeService
    auditer         <- destinationModule.destinationAuditer
  } yield {
    new FormBundleSubmissionService(
      formModule.fOptFormService,
      formTemplateModule.fOptFormTemplateAlgebra,
      destinationModule.destinationsProcessorModelService,
      destinationsSubmitter,
      RepoAlgebra.fOpt(submissionRepo),
      formTreeService,
      auditer,
      auditer
    )(fOptMonadError)
  }

  val formBundleController = new FormBundleController(formBundleSubmissionService)
}
