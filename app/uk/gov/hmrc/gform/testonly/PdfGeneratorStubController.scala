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

package uk.gov.hmrc.gform.testonly

import java.io.ByteArrayOutputStream

import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import org.apache.pdfbox.pdmodel.{ PDDocument, PDPage }

import scala.concurrent.ExecutionContext

class PdfGeneratorStubController(controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  def generate() = Action {
    val document = new PDDocument()
    val blankPage = new PDPage()
    document.addPage(blankPage)
    val stream = new ByteArrayOutputStream()
    document.save(stream)
    document.close

    Ok(stream.toByteArray).as("application/pdf")
  }
}
