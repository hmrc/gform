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
import java.text.Format
import java.util

import io.github.cloudify.scala.spdf._
import java.io._
import java.net._
import javax.swing.text.html.HTML

import org.apache.pdfbox.pdmodel.PDPageContentStream
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.{ PDFont, PDTrueTypeFont, PDType1Font }
import org.apache.pdfbox.pdmodel.{ PDDocument, PDPage }
import play.api.libs.json.{ Json, Reads }
import play.twirl.api.Html
import uk.gov.hmrc.bforms.model.KeyPair
import uk.gov.hmrc.play.http.BadRequestException

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

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

object PDFBoxExample {

  def generate(message: String) = {
    println("InsideGenerate")
    var text = message
    //.split("(")(1) //.split(")")(0)
    //    println("////////////////////////////////////////////////////////")
    //    println("TEXT :" + text)
    //    println("////////////////////////////////////////////////////////")
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val doc = new PDDocument()
    try {

      //      val lines = new util.ArrayList[String]

      val page = new PDPage()
      doc.addPage(page)

      val contents = new PDPageContentStream(doc, page)

      val font: PDFont = PDType1Font.HELVETICA_BOLD
      val fontSize: Float = 12
      val leading: Float = 25

      val mediaBox: PDRectangle = page.getMediaBox
      val lines = jsonParser(message)

      val margin: Float = 72
      val width: Float = mediaBox.getWidth - 2 * margin
      val startX: Float = mediaBox.getLowerLeftX + margin
      val startY: Float = mediaBox.getUpperRightY - margin

      var lastSpace: Int = -1

      contents.beginText()
      contents.setFont(font, fontSize)
      contents.newLineAtOffset(startX, startY)
      for (line <- lines) {
        contents.showText(line)
        contents.newLineAtOffset(0, -leading)
      }
      contents.endText()
      contents.close()

      //      val output = new java.io.File("confirmation.pdf")
      //      doc.save(output)

      doc.save(byteArrayOutputStream)

    } finally {
      doc.close()
    }

    byteArrayOutputStream.toByteArray()
  }

  def jsonParser(formData: String): ArrayBuffer[String] = {
    val lines = new ArrayBuffer[String]
    val json = Json.parse(formData)
    val formTypeId = json \ "formTypeId"
    val verion = json \ "version"
    val fields = (json \ "fields").as[List[KeyPair]]
    //      println("/////////////////////////////////////////////////////////////")
    //      println(json)
    //      println("/////////////////////////////////////////////////////////////")
    lines.+=(s"formTypeId : ${formTypeId.get}")
    lines.+=(s"version : ${verion.get}")
    lines.+=(s"fields :")

    for (elem <- fields) {
      lines.+=(s" ${elem.id}  : ${elem.value}")
    }
    lines
  }

}

object SPDFExample {
  def generateSpdf(form: String) = {
    val outputStream = new ByteArrayOutputStream()
    val pdf = WrappedPdf(Seq("xvfb-run", "wkhtmltopdf"), new PdfConfig {
      orientation := Landscape
      pageSize := "Letter"
      marginTop := "1in"
      marginBottom := "1in"
      marginLeft := "1in"
      marginRight := "1in"

    })
    val page = <html><body><h1>Hello World</h1></body></html>
    Array(pdf.run(page, outputStream).toByte)
  }
}

