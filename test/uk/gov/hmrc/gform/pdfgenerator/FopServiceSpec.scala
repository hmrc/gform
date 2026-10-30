/*
 * Copyright 2026 HM Revenue & Customs
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

import java.io.File
import org.apache.pdfbox.Loader
import org.apache.pdfbox.text.PDFTextStripper
import org.scalatest.time.{ Seconds, Span }
import play.api.{ Environment, Mode }
import uk.gov.hmrc.gform.Spec

import scala.jdk.CollectionConverters._
import scala.util.Using

class FopServiceSpec extends Spec {
  private val service = new FopService(new Environment(new File("."), getClass.getClassLoader, Mode.Test))
  private val sample =
    "Arial ABCDEFG abcdefg 0123456789 Příliš žluťoučký kůň ȘșȚț"

  Seq("normal" -> "ArialMT", "bold" -> "Arial-BoldMT").foreach { case (weight, fontName) =>
    "render" should s"use non-embedded $fontName and preserve Czech and Romanian accented characters" in {
      val content =
        s"""<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format" xml:lang="en" font-family="Arial">
           |  <fo:layout-master-set>
           |    <fo:simple-page-master master-name="page" page-width="210mm" page-height="297mm">
           |      <fo:region-body/>
           |    </fo:simple-page-master>
           |  </fo:layout-master-set>
           |  <fo:page-sequence master-reference="page">
           |    <fo:flow flow-name="xsl-region-body">
           |      <fo:block font-weight="$weight">$sample</fo:block>
           |    </fo:flow>
           |  </fo:page-sequence>
           |</fo:root>""".stripMargin

      val bytes = service.render(content).futureValue(timeout(Span(10, Seconds)))

      Using.resource(Loader.loadPDF(bytes)) { document =>
        val resources = document.getPage(0).getResources
        val fonts = resources.getFontNames.asScala.map(resources.getFont).toList

        fonts.map(_.getName).toSet shouldBe Set(fontName)
        all(fonts.map(_.isEmbedded)) shouldBe false
        new PDFTextStripper().getText(document).trim shouldBe sample
      }
    }
  }
}
