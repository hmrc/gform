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

import cats.data.NonEmptyList
import cats.syntax.either._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.bforms.core._
import uk.gov.hmrc.bforms.exceptions.InvalidState

case class FieldValue(
  id: FieldId,
  `type`: ComponentType,
  label: String,
  value: Option[Expr],
  helpText: Option[String],
  readOnly: Option[String],
  mandatory: Boolean
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
    `type`: Option[ComponentTypeRaw],
    label: String,
    value: Option[Expr],
    format: Option[FormatExpr],
    helpText: Option[String],
    readOnly: Option[String],
    choices: Option[List[String]],
    mandatory: Option[String],
    offset: Option[Offset],
    multivalue: Option[String]
) {
  private def getFieldValue(mandatory: Boolean): JsResult[FieldValue] = {
    val fieldValueOpt = for {
      componentType <- toComponentType
    } yield FieldValue(
      id = id,
      `type` = componentType,
      label = label,
      value = value,
      helpText = helpText,
      readOnly = readOnly,
      mandatory = mandatory
    )

    fieldValueOpt match {
      case Right(fieldValue) => JsSuccess(fieldValue)
      case Left(error) => JsError(error.toString)
    }
  }

  private val toComponentType: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None => Right(Text)
    case Some(DateRaw) => Right(Date(format.getOrElse(GeneralDate), offset.getOrElse(OffsetCase(0))))
    case Some(AddressRaw) => Right(Address)
    case Some(ChoiceRaw) =>
      (format, choices, multivalue) match {
        case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes)) => Right(Choice(Checkbox, NonEmptyList(x, xs), Vertical))
        case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo)) => Right(Choice(Radio, NonEmptyList(x, xs), Vertical))
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes)) => Right(Choice(Checkbox, NonEmptyList(x, xs), Horizontal))
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo)) => Right(Choice(Radio, NonEmptyList(x, xs), Horizontal))
        case (IsOrientation(YesNoOrientation), None, IsMultivalue(MultivalueNo)) => Right(Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal))
        case (invalidFormat, invalidChoices, invalidMultivalue) => Left(
          InvalidState(s"""|Unsupported combination of 'format, choices and multivalue':
                           |Format     : $invalidFormat
                           |Choices    : $invalidChoices
                           |Multivalue : $invalidMultivalue
                           |""".stripMargin)
        )
      }
  }

  private sealed trait OrientationValue

  private final case object VerticalOrientation extends OrientationValue
  private final case object HorizontalOrientation extends OrientationValue
  private final case object YesNoOrientation extends OrientationValue

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[OrientationValue] = {
      orientation match {
        case Some(TextExpression("vertical")) | None => Some(VerticalOrientation)
        case Some(TextExpression("horizontal")) => Some(HorizontalOrientation)
        case Some(TextExpression("yesno")) => Some(YesNoOrientation)
        case _ => None
      }
    }
  }

  private sealed trait Multivalue

  private final case object MultivalueYes extends Multivalue
  private final case object MultivalueNo extends Multivalue

  private final object IsMultivalue {
    def unapply(multivalue: Option[String]): Option[Multivalue] = {
      multivalue match {
        case Some(IsFalseish()) | None => Some(MultivalueNo)
        case Some(IsTrueish()) => Some(MultivalueYes)
        case _ => None
      }
    }
  }

  def toFieldValue = Reads[FieldValue] { _ =>
    mandatory match {
      case Some(IsTrueish()) | None => getFieldValue(true)
      case Some(IsFalseish()) => getFieldValue(false)
      case otherwise => JsError(s"Expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise")
    }
  }

  object IsTrueish {
    def unapply(maybeBoolean: String): Boolean = {
      maybeBoolean.toLowerCase match {
        case "true" | "yes" => true
        case _ => false
      }
    }
  }
  object IsFalseish {
    def unapply(maybeBoolean: String): Boolean = {
      maybeBoolean.toLowerCase match {
        case "false" | "no" => true
        case _ => false
      }
    }
  }

}
