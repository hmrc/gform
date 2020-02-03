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

package uk.gov.hmrc.gform.dms

import java.time.Clock

import cats.instances.future._
import org.apache.pdfbox.pdmodel.PDDocument
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.fileupload.{ FileUploadFrontendConnector, FileUploadModule }
import uk.gov.hmrc.gform.pdfgenerator.PdfGeneratorModule

import scala.concurrent.{ ExecutionContext, Future }

class DmsModule(
  fileUploadModule: FileUploadModule,
  fileUploadFrontendConnector: FileUploadFrontendConnector,
  pdfGeneratorModule: PdfGeneratorModule,
  config: AppConfig,
  controllerComponents: ControllerComponents)(implicit ex: ExecutionContext) {

  private lazy val dmsSubmissionService: DmsSubmissionService[Future] =
    new DmsSubmissionService(
      fileUploadModule.fileUploadService,
      fileUploadFrontendConnector,
      pdfGeneratorModule.pdfGeneratorService,
      PDDocument.load,
      config.formExpiryDays.longValue
    )(Clock.systemDefaultZone, catsStdInstancesForFuture)

  lazy val dmsSubmissionController: DmsSubmissionController = {
    new DmsSubmissionController(controllerComponents, dmsSubmissionService)
  }
}
