/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.translation.audit

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.time.Instant
import java.util.Base64
import julienrf.json.derived
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import play.api.libs.json.{ Format, JsError, JsObject, JsString, JsSuccess, Json, OFormat, Reads, Writes }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

final case class TranslationAuditOverview(
  _id: TranslationAuditId,
  formTemplateId: FormTemplateId,
  result: TranslationResult,
  createdAt: Instant
)

object TranslationAuditOverview {
  implicit val format: OFormat[TranslationAuditOverview] = {
    implicit val instantReads = uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.instantReads
    Json.format[TranslationAuditOverview] // Reads from Mongo, Writes is derived
  }
}

final case class TranslationAudit(
  formTemplateId: FormTemplateId,
  jsonBefore: JsObject,
  jsonAfter: JsObject,
  spreadsheet: XSSFWorkbook,
  result: TranslationResult,
  createdAt: Instant
)

object TranslationAudit {
  implicit val xssfWorkbookFormat: Format[XSSFWorkbook] = Format[XSSFWorkbook](
    Reads[XSSFWorkbook] {
      case JsString(str) =>
        val payload: Array[Byte] = Base64.getDecoder.decode(str)

        val stream: ByteArrayInputStream = new ByteArrayInputStream(payload);

        val workBook = new XSSFWorkbook(stream)

        JsSuccess(workBook)
      case other => JsError(s"Invalid json, not found '$other'")
    },
    Writes[XSSFWorkbook] { spreadsheet =>
      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      spreadsheet.write(baos)
      val payload: String = Base64.getEncoder.encodeToString(baos.toByteArray())
      baos.close()
      JsString(payload)
    }
  )

  implicit val format: OFormat[TranslationAudit] = {
    implicit val instantFormat = uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.instantFormat
    derived.oformat()
  }
}

sealed trait TranslationResult extends Product with Serializable

object TranslationResult {
  case object Success extends TranslationResult
  case class Failure(reason: String) extends TranslationResult

  implicit val format: OFormat[TranslationResult] = derived.oformat()
}
