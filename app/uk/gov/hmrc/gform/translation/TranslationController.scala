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

import java.io.{ BufferedOutputStream, BufferedReader, ByteArrayInputStream, ByteArrayOutputStream, InputStream, InputStreamReader }
import play.api.libs.json.{ JsObject, Json }
import org.apache.pekko.stream.scaladsl.StreamConverters
import play.api.mvc.{ Action, AnyContent, ControllerComponents, Result }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.formtemplate.{ FormTemplateService, FormTemplatesControllerRequestHandler }
import uk.gov.hmrc.gform.history.HistoryService
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateRaw, FormTemplateRawId }
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

  //Ignore missing fonts in headless environments (for XSLX generation)
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
  ): Action[AnyContent] = generateXlsx(formTemplateId, TextExtractor.generateTranslatableCvsFromString)

  def generateBriefTranslatebleXlsx(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateXlsx(formTemplateId, TextExtractor.generateBriefTranslatableCvsFromString)

  def generateInternalCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] = generateCsv(formTemplateId, TextExtractor.generateCvsFromString)

  private def generateCsv(
    formTemplateId: FormTemplateId,
    generate: (String, BufferedOutputStream) => Unit
  ): Action[AnyContent] =
    Action.async { request =>
      formTemplateService
        .get(FormTemplateRawId(formTemplateId.value))
        .map { json =>
          val jsonAsString = Json.prettyPrint(json.value)
          Ok.chunked(StreamConverters.fromInputStream(() => fileByteData(jsonAsString, generate)))
            .withHeaders(
              CONTENT_TYPE        -> "text/csv",
              CONTENT_DISPOSITION -> s"""attachment; filename="${formTemplateId.value}.csv""""
            )
        }
    }

  private def generateXlsx(
    formTemplateId: FormTemplateId,
    generate: (String, BufferedOutputStream) => Unit
  ): Action[AnyContent] =
    Action.async { _ =>
      formTemplateService
        .get(FormTemplateRawId(formTemplateId.value))
        .map { json =>
          val jsonAsString = Json.prettyPrint(json.value)
          Ok.chunked(StreamConverters.fromInputStream(() => convertCsvToXlsx(fileByteData(jsonAsString, generate))))
            .withHeaders(
              CONTENT_TYPE        -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              CONTENT_DISPOSITION -> s"""attachment; filename="${formTemplateId.value}.xlsx""""
            )
        }
    }

  def convertCsvToXlsx(inputStream: ByteArrayInputStream) =
    Try(new BufferedReader(new InputStreamReader(inputStream))).map { br =>
      val workBook: XSSFWorkbook = new XSSFWorkbook()
      val sheet: XSSFSheet = workBook.createSheet("translations")
      var currentLine: String = null
      var rowNumber: Int = 0
      while ({ currentLine = br.readLine(); currentLine != null }) {
        val rowArray: Array[String] = currentLine.split(",")
        rowNumber += 1
        val currentRow: XSSFRow = sheet.createRow(rowNumber);
        var i = 0
        while (i < rowArray.length) {
          currentRow.createCell(i).setCellValue(rowArray(i))
          i += 1
        }
      }
      val bos: ByteArrayOutputStream = new ByteArrayOutputStream()
      workBook.write(bos)
      val bArray: Array[Byte] = bos.toByteArray()
      val is: InputStream = new ByteArrayInputStream(bArray)
      workBook.close()
      is
    } match {
      case Success(sucessIs) => sucessIs
      case Failure(_)        => throw new Exception("Failed to convert CSV to XLSX")
    }

  def translateCsv(
    formTemplateId: FormTemplateId
  ): Action[AnyContent] =
    Action.async { request =>
      val maybeCsv: Option[String] = request.body.asText

      maybeCsv.fold[Future[Result]](
        BadRequest("No csv file provided. Please make sure to use 'Content-type: text/plain'").pure[Future]
      ) { csv =>
        formTemplateService
          .get(FormTemplateRawId(formTemplateId.value))
          .map(json => insertLanguages(json.value))
          .flatMap { json =>
            val jsonAsString = Json.prettyPrint(json)
            val jsonToSave = Json.parse(TextExtractor.translateFile(csv, jsonAsString)).as[JsObject]
            interpreter
              .handleRequest(FormTemplateRaw(jsonToSave))
              .fold(_.asBadRequest, _ => Ok(jsonToSave))
          }
      }
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
