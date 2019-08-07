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

import play.api.libs.json.JsValue
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.core.{ FOpt, fromFutureA }
import uk.gov.hmrc.gform.email.EmailModule
import uk.gov.hmrc.gform.fileupload.{ FileDownloadAlgebra, FileUploadModule }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateModule
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsHttpApiModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.logging.Loggers
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class SubmissionModule(
  configModule: ConfigModule,
  mongoModule: MongoModule,
  pdfGeneratorModule: PdfGeneratorModule,
  formService: FormAlgebra[Future],
  formTemplateModule: FormTemplateModule,
  fileUploadModule: FileUploadModule,
  wsHttpModule: WSHttpModule,
  timeModule: TimeModule,
  emailModule: EmailModule,
  handlebarsHttpApiModule: HandlebarsHttpApiModule)(implicit ex: ExecutionContext) {

  private val fOptFormAlgebra: FormAlgebra[FOpt] = new FormAlgebra[FOpt] {
    override def get(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Form] = fromFutureA(formService.get(formId))

    override def delete(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.delete(formId))

    override def create(
      userId: UserId,
      formTemplateId: FormTemplateId,
      affinityGroup: Option[AffinityGroup],
      expiryDays: Long,
      initialFields: Seq[FormField])(implicit hc: HeaderCarrier): FOpt[NewFormData] =
      fromFutureA(formService.create(userId, formTemplateId, affinityGroup, expiryDays, initialFields))

    override def updateUserData(formId: FormId, userData: UserData)(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.updateUserData(formId, userData))

    def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): FOpt[FormStatus] =
      fromFutureA(formService.updateFormStatus(formId, newStatus))

    override def saveKeyStore(formId: FormId, data: Map[String, JsValue])(implicit hc: HeaderCarrier): FOpt[Unit] =
      fromFutureA(formService.saveKeyStore(formId, data))

    override def getKeyStore(formId: FormId)(implicit hc: HeaderCarrier): FOpt[Option[Map[String, JsValue]]] =
      fromFutureA(formService.getKeyStore(formId))
  }

  //TODO: this should be replaced with save4later for submissions

  val submissionRepo = new SubmissionRepo(mongoModule.mongo)

  private val fileUploadServiceDmsSubmitter = new FileUploadServiceDmsSubmitter(
    fileUploadModule.fileUploadService,
    fOptFormAlgebra,
    formTemplateModule.fOptFormTemplateAlgebra,
    pdfGeneratorModule.pdfGeneratorService
  )

  private val destinationAuditer: DestinationAuditAlgebra[FOpt] =
    if (configModule.DestinationsServicesConfig.auditDestinations) {
      Loggers.destinations.info("Destination auditing IS enabled")
      new RepoDestinationAuditer(
        new Repo[DestinationAudit]("destinationAudit", mongoModule.mongo, _.id.toString),
        new Repo[SummaryHtml]("summaryHtml", mongoModule.mongo, _.id.value.toString),
        fOptFormAlgebra
      )
    } else {
      Loggers.destinations.info("Destination auditing IS NOT enabled")
      new NullDestinationAuditer[FOpt]
    }

  private val realDestinationSubmitter = new RealDestinationSubmitter(
    fileUploadServiceDmsSubmitter,
    handlebarsHttpApiModule.handlebarsHttpSubmitter,
    destinationAuditer,
    fOptFormAlgebra
  )

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

  val submissionService = new SubmissionService(
    pdfGeneratorModule.pdfGeneratorService,
    fOptFormAlgebra,
    formTemplateModule.formTemplateService,
    new DestinationsSubmitter(realDestinationSubmitter, fileDownloadServiceIfPopulating),
    submissionRepo,
    emailModule.emailLogic,
    timeModule.timeProvider
  )

  val submissionController = new SubmissionController(submissionService)
}
