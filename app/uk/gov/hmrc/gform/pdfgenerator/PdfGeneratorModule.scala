/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.pdfgenerator

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.connectors.PdfGeneratorConnector
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.ExecutionContext

class PdfGeneratorModule(configModule: ConfigModule, wSHttpModule: WSHttpModule)(implicit ex: ExecutionContext) {

  val pdfGeneratorConnector = new PdfGeneratorConnector(configModule.serviceConfig, wSHttpModule.auditableWSHttp)

  val pdfGeneratorService = new PdfGeneratorService(pdfGeneratorConnector)

}
