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

package uk.gov.hmrc.gform.submission.destinations

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formmetadata.FormMetadataModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.objectstore.{ ObjectStoreAlgebra, ObjectStoreModule }
import uk.gov.hmrc.gform.repo.{ Repo, RepoAlgebra }
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, PdfContent }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class DestinationModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  formModule: FormModule,
  metadataModule: FormMetadataModule,
  objectStoreModule: ObjectStoreModule
)(implicit ex: ExecutionContext) {
  private val logger = LoggerFactory.getLogger(getClass)
  val destinationAuditer: Option[RepoDestinationAuditer] =
    if (configModule.DestinationsServicesConfig.auditDestinations) {
      logger.info("Destination auditing IS enabled")
      Some(
        new RepoDestinationAuditer(
          RepoAlgebra.fOpt(new Repo[DestinationAudit]("destinationAudit", mongoModule.mongoComponent, _.id.toString)),
          RepoAlgebra.fOpt(new Repo[SummaryHtml]("summaryHtml", mongoModule.mongoComponent, _.id.value.toString)),
          formModule.fOptFormService
        )
      )
    } else {
      logger.info("Destination auditing IS NOT enabled")
      None
    }

  val formTreeService: FormTreeAlgebra[FOpt] = new FormTreeService(metadataModule.foptFormMetadataService)

  private val objectStoreServiceIfPopulating: Option[ObjectStoreAlgebra[FOpt]] =
    if (configModule.DestinationsServicesConfig.populateHandlebarsModelWithDocuments) {
      logger.info(
        "The objectStoreService IS configured for the submission service, so the Handlebars model WILL be populated with uploaded documents"
      )
      Some(objectStoreModule.foptObjectStoreService)
    } else {
      logger.info(
        "The objectStoreService IS NOT configured for the submission service, so the Handlebars model WILL NOT be populated with uploaded documents"
      )
      None
    }

  val destinationsProcessorModelService: DestinationsProcessorModelAlgebra[FOpt] =
    new DestinationsProcessorModelService[FOpt](objectStoreServiceIfPopulating)

  val futureDestinationsProcessorModelService: DestinationsProcessorModelAlgebra[Future] =
    new DestinationsProcessorModelAlgebra[Future] {
      override def create(
        form: Form,
        frontEndSubmissionVariables: FrontEndSubmissionVariables,
        pdfData: PdfContent,
        instructionPdfHtml: Option[PdfContent],
        structuredFormData: StructuredFormValue.ObjectStructure
      )(implicit hc: HeaderCarrier): Future[HandlebarsTemplateProcessorModel] =
        destinationsProcessorModelService
          .create(form, frontEndSubmissionVariables, pdfData, instructionPdfHtml, structuredFormData)
          .toFuture
    }
}
