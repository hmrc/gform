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

import com.openhtmltopdf.pdfboxout.PdfRendererBuilder
import play.mvc.Http.{ HeaderNames, MimeTypes }
import uk.gov.hmrc.gform.connectors.PdfGeneratorConnector

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

import java.io.ByteArrayOutputStream

trait PdfGeneratorAlgebra[F[_]] {
  def generatePDFBytes(html: String)(implicit hc: HeaderCarrier): F[Array[Byte]]
  def generatePDFBytesLocal(html: String)(implicit ec: ExecutionContext): F[Array[Byte]]
}

class PdfGeneratorService(pdfGeneratorConnector: PdfGeneratorConnector) extends PdfGeneratorAlgebra[Future] {

  def generatePDFBytes(html: String)(implicit hc: HeaderCarrier): Future[Array[Byte]] = {
    val headers = Seq((HeaderNames.CONTENT_TYPE, MimeTypes.FORM))
    val body = Map("html" -> Seq(html), "force-pdfa" -> Seq("false"))
    pdfGeneratorConnector.generatePDF(body, headers)
  }

  def generatePDFBytesLocal(html: String)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val builder = new PdfRendererBuilder()
    builder.useFastMode();
    builder.withHtmlContent(html, null)
    builder.toStream(byteArrayOutputStream);
    builder.run()
    byteArrayOutputStream.toByteArray
  }
}
