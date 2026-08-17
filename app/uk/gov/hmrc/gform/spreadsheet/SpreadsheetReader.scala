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
import org.apache.poi.ss.usermodel.{ Cell, CellType }
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.xssf.usermodel._
import scala.jdk.CollectionConverters._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

object SpreadsheetReader {

  def readDataFromSpreadsheet(workbook: XSSFWorkbook): SpreadsheetData = {
    val firstSheetName: String = workbook.getSheetName(0)

    val sheet: XSSFSheet = workbook.getSheet(firstSheetName)

    val firstRowNumber = sheet.getFirstRowNum()

    val firstRow = sheet.getRow(firstRowNumber)

    val firstRowList: List[String] = firstRow
      .cellIterator()
      .asScala
      .map { cell =>
        cell.getStringCellValue().toLowerCase()
      }
      .toList

    val nameIndex0: Int = firstRowList.indexOf("name")
    val valueIndex0: Int = firstRowList.indexOf("value")

    val nameIndex = if (nameIndex0 == -1) 0 else nameIndex0 // If 'name' header is missing assume that it's first
    val valueIndex = if (valueIndex0 == -1) 1 else valueIndex0 // If 'value' header is missing assume that it's second

    val toDrop = if (nameIndex0 == -1) 0 else 1

    val spreadheetRows: Map[FormComponentId, String] = sheet
      .rowIterator()
      .asScala
      .drop(toDrop)
      .filter { row =>
        row.getLastCellNum =!= -1 // Ignore empty rows. Google Sheets or Libre are adding empty rows past the last translation
      }
      .map { row =>
        val name = getCellValue(row.getCell(nameIndex))
        val value = getCellValue(row.getCell(valueIndex))
        (FormComponentId(name), value)
      }
      .filterNot { case (FormComponentId(id), value) =>
        id.trim.isEmpty && value.trim.isEmpty // Drop empty lines
      }
      .toMap

    SpreadsheetData(spreadheetRows)
  }

  private def getCellValue(cell: Cell): String =
    if (cell == null) {
      "" // LibreOffice spreadsheets may return null for a cell
    } else {
      cell.getCellType() match {
        case CellType._NONE   => ""
        case CellType.BLANK   => ""
        case CellType.BOOLEAN => cell.getBooleanCellValue().toString
        case CellType.ERROR   => ""
        case CellType.FORMULA => ""
        case CellType.NUMERIC => cell.getNumericCellValue().toString
        case CellType.STRING  => cell.getStringCellValue()
      }
    }
}
