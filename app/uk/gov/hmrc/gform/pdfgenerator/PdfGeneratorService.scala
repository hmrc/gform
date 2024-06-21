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

package uk.gov.hmrc.gform.pdfgenerator

import com.openhtmltopdf.pdfboxout.PdfRendererBuilder

import java.io.ByteArrayOutputStream
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Using

trait PdfGeneratorAlgebra[F[_]] {
  def generatePDFBytesLocal(html: String)(implicit ec: ExecutionContext): F[Array[Byte]]
}

class PdfGeneratorService extends PdfGeneratorAlgebra[Future] {

  def generatePDFBytesLocal(html: String)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future {
    Using.resource(new ByteArrayOutputStream()) { byteArrayOutputStream =>
      val builder = new PdfRendererBuilder()
      builder.useFastMode()
      builder.withHtmlContent(html, null)
      builder.toStream(byteArrayOutputStream)
      builder.run()
      byteArrayOutputStream.toByteArray
    }
  }
}
