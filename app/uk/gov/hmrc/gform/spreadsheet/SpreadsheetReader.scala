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

package uk.gov.hmrc.gform.spreadsheet

import cats.implicits._
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.xssf.usermodel._
import scala.jdk.CollectionConverters._

object SpreadsheetReader {

  def readDataFromSpreadsheet(workbook: XSSFWorkbook): SpreadsheetData = {
    val firstSheetName: String = workbook.getSheetName(0)

    val sheet: XSSFSheet = workbook.getSheet(firstSheetName)

    val formatter = new org.apache.poi.ss.usermodel.DataFormatter()

    val spreadheetRows: List[List[String]] = sheet
      .rowIterator()
      .asScala
      .filter { row =>
        row.getLastCellNum =!= -1 // Ignore empty rows. Google Sheets or Libre are adding empty rows past the last translation
      }
      .map { row =>
        val xs: Seq[String] = for {
          colIndex <- 0 until row.getLastCellNum().toInt
        } yield {
          val cell = row.getCell(colIndex)

          if (cell == null) ""
          else formatter.formatCellValue(cell)
        }
        xs.toList
      }
      .filterNot(
        _.forall(_.trim.isEmpty) // Drop empty lines
      )
      .toList

    // Spreadsheet may not store trailing empty cells, so rows can be shorter than the others.
    // Pad them to the widest row, so every row has the same number of columns.
    val width = spreadheetRows.map(_.size).maxOption.getOrElse(0)
    val paddedRows = spreadheetRows.map(_.padTo(width, ""))

    SpreadsheetData(paddedRows)
  }
}
