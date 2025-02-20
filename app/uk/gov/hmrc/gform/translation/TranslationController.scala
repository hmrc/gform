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

package uk.gov.hmrc.gform.translation

import cats.implicits._

import java.io.{ BufferedOutputStream, ByteArrayInputStream, ByteArrayOutputStream }
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.{ JsObject, Json }
import org.apache.pekko.stream.scaladsl.StreamConverters
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Result }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, FormTemplatesControllerRequestHandler }
import uk.gov.hmrc.gform.history.HistoryService
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRaw, FormTemplateRawId }
import org.apache.poi.ss.usermodel.{ Cell, CellType }
import org.apache.poi.xssf.usermodel._

import scala.util.{ Failure, Success, Try }

class TranslationController(
  formTemplateService: FormTemplateService,
  historyService: HistoryService,
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  private val interpreter = new FormTemplatesControllerRequestHandler(
    formTemplateService.verifyAndSave,
    formTemplateService.save,
    historyService.save
  ).futureInterpreter

  //Ignore missing fonts in headless environments (for XLSX generation)
  System.setProperty("org.apache.poi.ss.ignoreMissingFontSystem", "true")

  private def fileByteData(json: String, generate: (String, BufferedOutputStream) => Unit): ByteArrayInputStream = {

    val baos = new ByteArrayOutputStream()
    val bos = new BufferedOutputStream(baos)

    generate(json, bos)

    new ByteArrayInputStream(baos.toByteArray)
  }

  def generateTranslatebleCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateCsv(formTemplateId, TextExtractor.generateTranslatableCvsFromString)

  def generateBriefTranslatebleCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateCsv(formTemplateId, TextExtractor.generateBriefTranslatableCvsFromString)

  def generateTranslatebleXlsx(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateXlsx(formTemplateId, TextExtractor.generateTranslatableRows)

  def generateBriefTranslatebleXlsx(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateXlsx(
    formTemplateId,
    (s: String) =>
      ("en", "cy") :: TextExtractor
        .generateBriefTranslatableRows(s)
        .map(textTotransalate => (textTotransalate.en, ""))
  )

  def generateInternalCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateCsv(formTemplateId, TextExtractor.generateCvsFromString)

  def textBreakdown(formTemplateId: FormTemplateId): Action[AnyContent] =
    withFormTemplate(formTemplateId) { json =>
      val jsonAsString = Json.prettyPrint(json.value)
      val translatableRows: List[TranslatedRow] = TextExtractor.generateTranslatableRows(jsonAsString).map {
        case (en, cy) => TranslatedRow(en, cy)
      }

      val enTextBreakdowns: List[EnTextBreakdown] = translatableRows.flatMap { translatableRow =>
        val breakdown: List[EnTextToTranslate] =
          ExtractAndTranslate(translatableRow.en).translateTexts

        if (breakdown.size > 1) {
          Some(EnTextBreakdown(translatableRow.en, breakdown.map(_.en)))
        } else None
      }

      Ok(Json.toJson(EnTextBreakdowns(enTextBreakdowns)))
    }

  private def withFormTemplate(formTemplateId: FormTemplateId)(
    f: FormTemplateRaw => Result
  ): Action[AnyContent] =
    Action.async { _ =>
      formTemplateService
        .get(FormTemplateRawId(formTemplateId.value))
        .map(f)
    }

  private def generateCsv(
    formTemplateId: FormTemplateId,
    generate: (String, BufferedOutputStream) => Unit
  ): Action[AnyContent] =
    withFormTemplate(formTemplateId) { json =>
      val jsonAsString = Json.prettyPrint(json.value)
      Ok
        .chunked(StreamConverters.fromInputStream(() => fileByteData(jsonAsString, generate)))
        .withHeaders(
          CONTENT_TYPE        -> "text/csv",
          CONTENT_DISPOSITION -> s"""attachment; filename="${formTemplateId.value}.csv""""
        )
    }

  private def generateXlsx(
    formTemplateId: FormTemplateId,
    generate: String => List[(String, String)]
  ): Action[AnyContent] =
    withFormTemplate(formTemplateId) { json =>
      val jsonAsString = Json.prettyPrint(json.value)
      Ok.chunked(StreamConverters.fromInputStream(() => generateXlsx(generate(jsonAsString))))
        .withHeaders(
          CONTENT_TYPE        -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          CONTENT_DISPOSITION -> s"""attachment; filename="${formTemplateId.value}.xlsx""""
        )
    }

  def generateXlsx(rows: List[(String, String)]) = {
    val workbookTry = Try {
      val workBook = new XSSFWorkbook()
      val sheet = workBook.createSheet("translations")
      sheet.setColumnWidth(0, 100 * 256)
      sheet.setColumnWidth(1, 100 * 256)
      val style = workBook.createCellStyle()
      style.setWrapText(true)

      def processRow(en: String, cy: String, rowNumber: Int): Unit = {
        val row = sheet.createRow(rowNumber)
        List(en, cy).zipWithIndex.foreach { case (value, idx) =>
          val cell = row.createCell(idx)
          cell.setCellStyle(style)
          cell.setCellValue(value)
        }
      }

      rows.zipWithIndex.foreach { case ((en, cy), idx) =>
        processRow(en, cy, idx)
      }

      val byteArrayOutputStream = new ByteArrayOutputStream()
      workBook.write(byteArrayOutputStream)
      workBook.close()

      new ByteArrayInputStream(byteArrayOutputStream.toByteArray)
    }

    workbookTry match {
      case Success(inputStream) => inputStream
      case Failure(e)           => throw new Exception("Failed to generate XLSX", e)
    }
  }

  def translateXlsx(
    formTemplateId: FormTemplateId
  ): Action[TemporaryFile] =
    Action.async(parse.temporaryFile) { request =>
      import scala.jdk.CollectionConverters._
      val temporalFile: play.api.libs.Files.TemporaryFile = request.body

      val path: java.nio.file.Path = temporalFile.path

      val reader: java.io.InputStream = java.nio.file.Files.newInputStream(path);

      val workBook = new XSSFWorkbook(reader)

      val firstSheetName = workBook.getSheetName(0) // Let's hope "translation" is in a first sheet

      val translations: XSSFSheet = workBook.getSheet(firstSheetName)

      val firstRowNumber = translations.getFirstRowNum()

      val firstRow = translations.getRow(firstRowNumber)

      val firstRowList: List[String] = firstRow
        .cellIterator()
        .asScala
        .map { cell =>
          cell.getStringCellValue()
        }
        .toList

      val enIndex: Int = firstRowList.indexOf("en")
      val cyIndex: Int = firstRowList.indexOf("cy")

      val spreadheetRows: Map[EnFromSpreadsheet, CyFromSpreadsheet] = translations
        .rowIterator()
        .asScala
        .drop(1)
        .filter { row =>
          row.getLastCellNum =!= -1 // Ignore empty rows. Google Sheets or Libre are adding empty rows past the last translation
        }
        .map { row =>
          val en = getCellValue(row.getCell(enIndex))
          val cy = getCellValue(row.getCell(cyIndex))
          (EnFromSpreadsheet(en), CyFromSpreadsheet(cy))
        }
        .toMap

      val spreadsheet = Spreadsheet(spreadheetRows)

      runTranslation(formTemplateId, spreadsheet)
    }

  private def getCellValue(cell: Cell): String =
    cell.getCellType() match {
      case CellType._NONE   => ""
      case CellType.BLANK   => ""
      case CellType.BOOLEAN => cell.getBooleanCellValue().toString
      case CellType.ERROR   => ""
      case CellType.FORMULA => ""
      case CellType.NUMERIC => cell.getNumericCellValue().toString
      case CellType.STRING  => cell.getStringCellValue()
    }

  def translateCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] =
    Action.async { request =>
      val maybeCsv: Option[String] = request.body.asText

      maybeCsv.fold[Future[Result]](
        BadRequest("No csv file provided. Please make sure to use 'Content-type: text/plain'").pure[Future]
      ) { csv =>
        val translatableRows = TextExtractor.readCvsFromString(csv)
        runTranslation(formTemplateId, translatableRows)
      }
    }

  private def runTranslation(formTemplateId: FormTemplateId, spreadsheet: Spreadsheet) =
    formTemplateService
      .get(FormTemplateRawId(formTemplateId.value))
      .map(json => insertLanguages(json.value))
      .flatMap { json =>
        val jsonAsString = Json.prettyPrint(json)
        val (translatedJson, stats) = TextExtractor.translateFile(spreadsheet, jsonAsString)
        val jsonToSave = Json.parse(translatedJson).as[JsObject]
        interpreter
          .handleRequest(FormTemplateRaw(jsonToSave))
          .fold(_.asBadRequest, _ => Ok(stats.spaces2))
      }

  private def insertLanguages(json: JsObject): JsObject = {
    val fields = json.fields
    val languages = ("languages", Json.toJson(Seq("en", "cy")))
    val updatedFields = if (fields.exists { case (key, _) => key === "languages" }) {
      fields.map {
        case (key, _) if key === "languages" => languages
        case otherwise                       => otherwise
      }
    } else {
      val (before, after) = fields.span(_._1 =!= "version")
      after match {
        case version :: as => before ++ Seq(version, languages) ++ as
        case _             => before ++ Seq(languages)
      }
    }
    JsObject(updatedFields)
  }

  def translateCsvDebug(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] =
    Action.async { request =>
      formTemplateService
        .get(FormTemplateRawId(formTemplateId.value))
        .map { json =>
          val jsonAsString = Json.prettyPrint(json.value)
          val outputJson: String = TextExtractor.debug(jsonAsString)
          Ok(Json.parse(outputJson))
        }
    }
}
