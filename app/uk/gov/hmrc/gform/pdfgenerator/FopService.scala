/*
 * Copyright 2024 HM Revenue & Customs
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

import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.fop.apps.{ FOUserAgent, Fop, FopConfParser, FopFactory, FopFactoryBuilder }
import org.apache.xmlgraphics.util.MimeConstants
import play.api.Environment

import java.io.{ File, StringReader }
import javax.xml.transform.{ Transformer, TransformerFactory }
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Using

trait FopAlgebra[F[_]] {
  def render(input: String)(implicit ec: ExecutionContext): F[Array[Byte]]
}

class FopService(environment: Environment) extends FopAlgebra[Future] {

  def render(input: String)(implicit ec: ExecutionContext): Future[Array[Byte]] = Future {
    Using.resource(new ByteArrayOutputStream()) { out =>
      val fopConfigFilePath: String = s"/conf/pdf/fop.xconf"
      val xconf: File = environment.getFile(fopConfigFilePath)
      val parser: FopConfParser = new FopConfParser(xconf) //parsing configuration

      val builder: FopFactoryBuilder = parser.getFopFactoryBuilder //building the factory with the user options
      val fopFactory: FopFactory = builder.build()

      // Turn on accessibility features
      val userAgent: FOUserAgent = fopFactory.newFOUserAgent()
      userAgent.setAccessibility(true)

      val fop: Fop = fopFactory.newFop(MimeConstants.MIME_PDF, userAgent, out)
      val transformerFactory: TransformerFactory = TransformerFactory.newInstance()
      val transformer: Transformer = transformerFactory.newTransformer()
      val source: StreamSource = new StreamSource(new StringReader(input))
      val result: SAXResult = new SAXResult(fop.getDefaultHandler)

      transformer.transform(source, result)
      out.toByteArray
    }
  }
}
