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

package uk.gov.hmrc.gform.bulkupload

import java.io.ByteArrayOutputStream
import org.apache.pekko.util.ByteString
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import scala.concurrent.Future
import scala.util.Using
import uk.gov.hmrc.gform.objectstore.ObjectStoreConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, SpreadsheetColumn }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.ObjectSummaryWithMd5

sealed trait BulkUploadService[F[_]] {
  def uploadXlsx(objectStoreData: BulkUploadObjectStoreData)(implicit
    hc: HeaderCarrier
  ): F[ObjectSummaryWithMd5]
  def generateXlsx(bulkUpload: BulkUpload): Array[Byte]
}

object BulkUploadService {
  def apply(
    objectStoreConnector: ObjectStoreConnector
  ): BulkUploadService[Future] = new BulkUploadService[Future] {
    def uploadXlsx(objectStoreData: BulkUploadObjectStoreData)(implicit
      hc: HeaderCarrier
    ): Future[ObjectSummaryWithMd5] = {

      val envelopeId = objectStoreData.envelopeId
      val fileName = objectStoreData.fileName

      objectStoreData.bulkUploadResult match {
        case BulkUploadResult.Ok(data) =>
          val bytes = buildWorkbook(objectStoreData.bulkUpload.spreadsheet, data)

          val file = ByteString.fromArray(bytes)

          objectStoreConnector.uploadFile(envelopeId, fileName, file, contentType = None, maybeSubDirectory = None)
        case BulkUploadResult.Error(error) =>
          throw new Exception(
            s"Error when uploading bulk upload xlsx to object store. Instead of rows data error received $error for $envelopeId"
          )
      }
    }

    def generateXlsx(bulkUpload: BulkUpload): Array[Byte] =
      buildWorkbook(bulkUpload.spreadsheet, List.empty[List[String]])
  }

  private def buildWorkbook(columns: List[SpreadsheetColumn], data: List[List[String]]): Array[Byte] =
    Using.resource(new XSSFWorkbook()) { workBook =>
      val sheet = workBook.createSheet("Sheet1")
      val style = workBook.createCellStyle()
      style.setWrapText(true)

      val headerRow = sheet.createRow(0)
      columns.zipWithIndex.foreach { case (column, idx) =>
        val cell = headerRow.createCell(idx)
        cell.setCellStyle(style)
        cell.setCellValue(column.title)
        // Width is in 1/256 of a character; Excel's maximum is 255 characters
        sheet.setColumnWidth(idx, math.min(column.title.length + 2, 255) * 256)
      }

      data.zipWithIndex.foreach { case (rowData, rowIdx) =>
        val row = sheet.createRow(rowIdx + 1)
        rowData.zipWithIndex.foreach { case (cellData, cellIdx) =>
          val cell = row.createCell(cellIdx)
          cell.setCellStyle(style)
          cell.setCellValue(cellData)
        }
      }

      val out = new ByteArrayOutputStream()
      workBook.write(out)
      out.toByteArray
    }

}
