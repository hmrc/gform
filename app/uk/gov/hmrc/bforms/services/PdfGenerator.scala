/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.services

import java.awt.Color
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream }
import org.apache.pdfbox.pdmodel.PDPageContentStream
import org.apache.pdfbox.pdmodel.font.{ PDFont, PDTrueTypeFont, PDType1Font }
import org.apache.pdfbox.pdmodel.{ PDDocument, PDPage }

object PdfGenerator {
  def generatePdf(message: String): Array[Byte] = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val doc = new PDDocument()
    try {
      val page = new PDPage()
      doc.addPage(page)

      val font: PDFont = PDType1Font.HELVETICA_BOLD

      val contents = new PDPageContentStream(doc, page)

      // fill the entire background with color
      contents.setNonStrokingColor(Color.GREEN)
      contents.addRect(0, 0, page.getMediaBox().getWidth(), page.getMediaBox().getHeight())
      contents.fill()

      // draw a color box in the lower left hand corner
      contents.setNonStrokingColor(Color.DARK_GRAY)
      contents.addRect(10, 10, 100, 100)
      contents.fill()

      contents.setNonStrokingColor(Color.BLACK)
      contents.beginText()
      contents.setFont(font, 12)
      contents.newLineAtOffset(100, 700)
      contents.showText(message)
      contents.endText()
      contents.close()

      //val output = new java.io.File("confirmation.pdf")
      //doc.save(output)

      doc.save(byteArrayOutputStream)
    } finally {
      doc.close()
    }

    byteArrayOutputStream.toByteArray()
  }
}
