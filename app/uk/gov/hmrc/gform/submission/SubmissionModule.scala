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

package uk.gov.hmrc.gform.submission

import org.apache.pekko.stream.Materializer
import org.mongodb.scala.model.{ IndexModel, IndexOptions }
import org.mongodb.scala.model.Indexes.ascending
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.envelope.EnvelopeModule
import uk.gov.hmrc.gform.hip.HipModule
import uk.gov.hmrc.gform.notifier.NotifierModule
import uk.gov.hmrc.gform.objectstore.ObjectStoreModule
import uk.gov.hmrc.gform.repo.{ Repo, RepoAlgebra }
import uk.gov.hmrc.gform.sdes.SdesModule
import uk.gov.hmrc.gform.submission.destinations.{ DataStoreSubmitter, DestinationModule, DestinationSubmitter, DestinationsSubmitter, DestinationsSubmitterAlgebra, DmsSubmitter, InfoArchiveSubmitter, NiRefundSubmitter, PegaSubmitter, StateTransitionService }
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorModule

import scala.concurrent.ExecutionContext

class SubmissionModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  pdfGeneratorModule: PdfGeneratorModule,
  formModule: FormModule,
  formTemplateModule: FormTemplateModule,
  timeModule: TimeModule,
  emailModule: EmailModule,
  submissionConsolidatorModule: SubmissionConsolidatorModule,
  handlebarsHttpApiModule: HandlebarsHttpApiModule,
  destinationModule: DestinationModule,
  notifierModule: NotifierModule,
  envelopeModule: EnvelopeModule,
  objectStoreModule: ObjectStoreModule,
  sdesModule: SdesModule,
  materializer: Materializer,
  hipModule: HipModule
)(implicit ex: ExecutionContext) {

  //TODO: this should be replaced with save4later for submissions

  val submissionRepo: Repo[Submission] = new Repo[Submission](
    "submission",
    mongoModule.mongoComponent,
    _._id.idString,
    Seq(
      IndexModel(ascending("formTemplateId"), IndexOptions().name("formTemplateIdIdx").background(true)),
      IndexModel(ascending("submittedDate"), IndexOptions().name("submittedDateIdx").background(true))
    )
  )

  private val fileUploadServiceDmsSubmitter = new DmsSubmitter(
    objectStoreModule.foptObjectStoreService,
    sdesModule.foptDestinationWorkItemService,
    formModule.fOptFormService,
    formTemplateModule.fOptFormTemplateAlgebra,
    pdfGeneratorModule.pdfGeneratorService,
    pdfGeneratorModule.fopService,
    envelopeModule.foptEnvelopeService,
    configModule.sdesConfig.welshDefaults
  )

  val dataStoreSubmitter = new DataStoreSubmitter(
    objectStoreModule.foptObjectStoreService,
    sdesModule.foptDestinationWorkItemService
  )

  val infoArchiveSubmitter = new InfoArchiveSubmitter(
    objectStoreModule.foptObjectStoreService,
    pdfGeneratorModule.pdfGeneratorService,
    pdfGeneratorModule.fopService,
    formTemplateModule.fOptFormTemplateAlgebra,
    sdesModule.foptDestinationWorkItemService,
    formModule.fOptFormService
  )

  private val pegaSubmitter = new PegaSubmitter(
    hipModule.getConnector
  )

  private val niRefundSubmitter = new NiRefundSubmitter(
    hipModule.getConnector
  )

  private val stateTransitionService = new StateTransitionService(formModule.fOptFormService)

  private val realDestinationSubmitter = new DestinationSubmitter(
    fileUploadServiceDmsSubmitter,
    handlebarsHttpApiModule.handlebarsHttpSubmitter,
    stateTransitionService,
    notifierModule.fOptNotifierService,
    destinationModule.destinationAuditer,
    submissionConsolidatorModule.submissionConsolidator,
    dataStoreSubmitter,
    infoArchiveSubmitter,
    configModule.sdesConfig,
    pegaSubmitterAlgebra = pegaSubmitter,
    niRefundSubmitterAlgebra = niRefundSubmitter
  )

  private val destinationsSubmitter: DestinationsSubmitterAlgebra[FOpt] = new DestinationsSubmitter(
    realDestinationSubmitter
  )

  val submissionService = new SubmissionService(
    formModule.fOptFormService,
    formTemplateModule.formTemplateService,
    destinationModule.destinationsProcessorModelService,
    destinationsSubmitter,
    submissionRepo,
    formTemplateModule.formRedirectService,
    emailModule.emailLogic,
    timeModule.timeProvider,
    envelopeModule.foptEnvelopeService
  )

  val submissionController = new SubmissionController(configModule.controllerComponents, submissionService)

  val formBundleSubmissionService: Option[FormBundleSubmissionService[FOpt]] = for {
    auditer <- destinationModule.destinationAuditer
  } yield new FormBundleSubmissionService(
    formModule.fOptFormService,
    formTemplateModule.fOptFormTemplateAlgebra,
    destinationModule.destinationsProcessorModelService,
    destinationsSubmitter,
    RepoAlgebra.fOpt(submissionRepo),
    destinationModule.formTreeService,
    auditer,
    auditer
  )(fOptMonadError)

  val formBundleController = new FormBundleController(configModule.controllerComponents, formBundleSubmissionService)
}
