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

package uk.gov.hmrc.bforms.models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.bforms.core.{ Format => FormatExpr, _ }

case class FieldValue(
  id: FieldId,
  `type`: ComponentType,
  label: String,
  value: Option[Expr],
  format: Option[FormatExpr],
  helpText: Option[String],
  readOnly: Option[String],
  mandatory: Boolean,
  offset: Option[Offset]
)

object FieldValue {

  private val formatFieldValueRaw: OFormat[FieldValueRaw] = Json.format[FieldValueRaw]

  implicit val format: OFormat[FieldValue] = {
    implicit val formatFieldValue = Json.format[FieldValue]

    val reads: Reads[FieldValue] = (formatFieldValue: Reads[FieldValue]) |
      (formatFieldValueRaw: Reads[FieldValueRaw]).flatMap(_.toFieldValue)

    OFormat[FieldValue](reads, formatFieldValue)
  }
}

private[this] case class FieldValueRaw(
    id: FieldId,
    `type`: Option[ComponentType],
    label: String,
    value: Option[Expr],
    format: Option[FormatExpr],
    helpText: Option[String],
    readOnly: Option[String],
    mandatory: Option[String],
    offset: Option[Offset]
) {

  private def getFieldValue(mandatory: Boolean) = FieldValue(
    id = id,
    `type` = `type`.getOrElse(Text),
    label = label,
    value = value,
    format = format,
    helpText = helpText,
    readOnly = readOnly,
    mandatory = mandatory,
    offset = offset
  )

  def toFieldValue = Reads[FieldValue] { _ =>
    mandatory match {
      case Some("true") | None => JsSuccess(getFieldValue(true))
      case Some("false") => JsSuccess(getFieldValue(false))
      case otherwise => JsError(s"Expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise")
    }
  }
}
