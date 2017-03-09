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
import java.io.ByteArrayOutputStream

import io.github.cloudify.scala.spdf._
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.{ PDFont, PDType1Font }
import org.apache.pdfbox.pdmodel.{ PDDocument, PDPage, PDPageContentStream }
import play.api.libs.json._
import uk.gov.hmrc.bforms.model.{ FieldValue, FormData, FormField, SectionFormField }

import scala.collection.mutable.ArrayBuffer

sealed trait LayoutElem {
  def count(c1: Int, c2: Int, c3: Int) = this match {
    case Text(_) => (this, c1 + 1, c2, c3)
    case SectionTitle(_) => (this, c1, c2 + 1, c3)
    case Line => (this, c1, c2, c3 + 1)
  }
}
case class Text(text: String) extends LayoutElem
case class SectionTitle(title: String) extends LayoutElem
case object Line extends LayoutElem

object PdfGenerator {

  def localisation(str: String): String = {
    str.split('|') match {
      case Array() => ""
      case Array(s, _*) => s.trim
    }
  }

  def generate(
    sectionFormFields: List[SectionFormField],
    formName: String
  ): Array[Byte] = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val doc = new PDDocument()
    try {

      val page = new PDPage()
      doc.addPage(page)

      val contents = new PDPageContentStream(doc, page)
      val mediaBox: PDRectangle = page.getMediaBox
      val margin: Float = 72
      val startX: Float = mediaBox.getLowerLeftX + margin
      val startY: Float = mediaBox.getUpperRightY - margin

      val layoutElems: List[LayoutElem] = sectionFormFields.flatMap { section =>
        val lines = section.fields.map {
          case (formField, fieldValue) =>
            Text(localisation(fieldValue.label) + " : " + formField.value)
        }
        SectionTitle(localisation(section.title)) :: lines ::: List(Line)
      }

      /**
       * First Int - number of text fields preceding this LayoutElem
       * Second Int - number of sections preceding this LayoutElem
       * Third Int - number of lines preceding this LayoutElem
       */
      val layoutElemsWithAccs: List[(LayoutElem, Int, Int, Int)] = layoutElems match {
        case Nil => Nil
        case x :: xs => xs.scanLeft((x, 0, 0, 0)) {
          case ((_, acc1, acc2, acc3), layoutElem) =>
            layoutElem.count(acc1, acc2, acc3)
        }
      }

      val offset = 40

      /**
       * Int denotes position of the LayoutElem as offset from the beginning of the page
       */
      val layoutElemsWithPosition: List[(LayoutElem, Int)] = layoutElemsWithAccs.map {
        case (p, count1, count2, count3) =>
          (p, offset + count1 * 20 + count2 * 30 + count3 * 10)
      }

      def renderTextOnPosition(text: String, position: Float, font: PDFont, fontSize: Float) = {
        contents.beginText()
        contents.setFont(font, fontSize)
        contents.newLineAtOffset(startX, startY - position)
        contents.showText(text)
        contents.endText()
      }

      renderTextOnPosition(localisation(formName), 0, PDType1Font.HELVETICA_BOLD, 20)

      layoutElemsWithPosition.map {
        case (Text(text), position) =>
          renderTextOnPosition(text, position, PDType1Font.HELVETICA_BOLD, 12)
        case (SectionTitle(title), position) =>
          renderTextOnPosition(title, position, PDType1Font.HELVETICA_OBLIQUE, 16)
        case (Line, position) =>
          contents.moveTo(margin, startY - position);
          contents.lineTo(mediaBox.getUpperRightX - margin, startY - position);
          contents.stroke();
      }

      contents.close()

      /* val output = new java.io.File("confirmation.pdf")
       * doc.save(output) */

      doc.save(byteArrayOutputStream)

    } finally {
      doc.close()
    }

    byteArrayOutputStream.toByteArray()
  }
}

case class EnvironmentalBodies(bodyName: String, amount: Int)

object EnvironmentalBodies {
  implicit val format: Reads[EnvironmentalBodies] = Json.reads[EnvironmentalBodies]
}

object PDFBoxExample {

  def generate(message: JsObject) = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val doc = new PDDocument()
    try {

      val page = new PDPage()
      doc.addPage(page)

      val contents = new PDPageContentStream(doc, page)
      val font: PDFont = PDType1Font.HELVETICA_BOLD
      val fontSize: Float = 12
      val leading: Float = 25
      val mediaBox: PDRectangle = page.getMediaBox
      val lines = jsonParser(message)
      val margin: Float = 72
      val startX: Float = mediaBox.getLowerLeftX + margin
      val startY: Float = mediaBox.getUpperRightY - margin

      contents.beginText()
      contents.setFont(font, fontSize)
      contents.newLineAtOffset(startX, startY)

      for (line <- lines) {
        contents.showText(line)

        contents.newLineAtOffset(0, -leading)
      }
      contents.endText()
      contents.close()

      doc.save(byteArrayOutputStream)

    } finally {
      doc.close()
    }

    byteArrayOutputStream.toByteArray()
  }

  def environmentalBodies(string: String): List[String] = {
    val list = new ArrayBuffer[String]()
    val int = string.indexOf('}')
    val subString: String = string.substring(0, int)
    list.+=(subString)
    if (string.substring(int).isEmpty) {
      list.++(environmentalBodies(string.substring(int)))
    }
    list.toList
  }

  def jsonParser(json: JsObject): ArrayBuffer[String] = {
    val lines = new ArrayBuffer[String]
    val formTypeId = json \ "formTypeId"
    val verion = json \ "version"
    val fields = (json \ "fields").as[List[FormField]]
    lines.+=(s"formTypeId : ${formTypeId.get}")
    lines.+=(s"version : ${verion.get}")
    lines.+=(s"fields :")

    for (elem <- fields) {
      if (elem.id == "environmentalBodies") {
        val envbody = Json.parse(elem.value).as[List[EnvironmentalBodies]]
        lines.+=("EnvironmentalBodies : - ")
        for (env <- envbody) {
          lines.+=(s"bodyName : ${env.bodyName}, amount : ${env.amount}")
        }
      } else {
        lines.+=(s"${elem.id}  : ${elem.value}")
      }
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

    val data = jsonParser(form)
    val page = uk.gov.hmrc.bforms.views.html.summary.render(data).body

    pdf.run(page, outputStream)

    outputStream.toByteArray
  }

  def jsonParser(formData: String): ArrayBuffer[String] = {
    val lines = new ArrayBuffer[String]
    val json = Json.parse(formData)
    val formTypeId = json \ "formTypeId"
    val verion = json \ "version"
    val fields = (json \ "fields").as[List[FormField]]
    lines.+=(s"formTypeId : ${formTypeId.get}")
    lines.+=(s"version : ${verion.get}")
    lines.+=(s"fields :")

    for (elem <- fields) {
      lines.+=(s" ${elem.id}  : ${elem.value}")
    }
    lines
  }
}
