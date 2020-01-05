/*
 * Copyright 2020 HM Revenue & Customs
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

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.fileupload.{ FileDownloadAlgebra, FileUploadModule }
import uk.gov.hmrc.gform.form.FormModule
import uk.gov.hmrc.gform.formmetadata.FormMetadataModule
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.repo.{ Repo, RepoAlgebra }
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.HandlebarsTemplateProcessorModel
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class DestinationModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  formModule: FormModule,
  fileUploadModule: FileUploadModule,
  metadataModule: FormMetadataModule)(implicit ex: ExecutionContext) {
  val destinationAuditer: Option[RepoDestinationAuditer] =
    if (configModule.DestinationsServicesConfig.auditDestinations) {
      Loggers.destinations.info("Destination auditing IS enabled")
      Some(
        new RepoDestinationAuditer(
          RepoAlgebra.fOpt(new Repo[DestinationAudit]("destinationAudit", mongoModule.mongo, _.id.toString)),
          RepoAlgebra.fOpt(new Repo[SummaryHtml]("summaryHtml", mongoModule.mongo, _.id.value.toString)),
          formModule.fOptFormService
        ))
    } else {
      Loggers.destinations.info("Destination auditing IS NOT enabled")
      None
    }

  val formTreeService: FormTreeAlgebra[FOpt] = new FormTreeService(metadataModule.foptFormMetadataService)

  private val fileDownloadServiceIfPopulating: Option[FileDownloadAlgebra[FOpt]] =
    if (configModule.DestinationsServicesConfig.populateHandlebarsModelWithDocuments) {
      Loggers.destinations.info(
        "The fileDownloadService IS configured for the submission service, so the Handlebars model WILL be populated with uploaded documents")
      Some(fileUploadModule.foptFileDownloadService)
    } else {
      Loggers.destinations.info(
        "The fileDownloadService IS NOT configured for the submission service, so the Handlebars model WILL NOT be populated with uploaded documents")
      None
    }

  val destinationsProcessorModelService: DestinationsProcessorModelAlgebra[FOpt] =
    new DestinationsProcessorModelService[FOpt](
      fileDownloadServiceIfPopulating
    )

  val futureDestinationsProcessorModelService: DestinationsProcessorModelAlgebra[Future] =
    new DestinationsProcessorModelAlgebra[Future] {
      override def create(
        form: Form,
        frontEndSubmissionVariables: FrontEndSubmissionVariables,
        pdfData: PdfHtml,
        structuredFormData: StructuredFormValue.ObjectStructure)(
        implicit hc: HeaderCarrier): Future[HandlebarsTemplateProcessorModel] =
        destinationsProcessorModelService
          .create(form, frontEndSubmissionVariables, pdfData, structuredFormData)
          .toFuture
    }
}
