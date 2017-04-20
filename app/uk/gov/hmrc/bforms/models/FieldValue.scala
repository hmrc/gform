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
    value: Option[ValueExpr],
    format: Option[FormatExpr],
    helpText: Option[String],
    readOnly: Option[String],
    choices: Option[List[String]],
    mandatory: Option[String],
    offset: Option[Offset],
    multivalue: Option[String],
    total: Option[String]
) {
  private def getFieldValue(mandatory: Boolean): JsResult[FieldValue] = {
    val fieldValueOpt = for {
      componentType <- toComponentType
    } yield FieldValue(
      id = id,
      `type` = componentType,
      label = label,
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
    case Some(TextRaw) | None =>
      (value, total) match {
        case (Some(TextExpression(expr)), IsTotal(TotalYes)) => Right(Text(expr, total = true))
        case (Some(TextExpression(expr)), IsTotal(TotalNo)) => Right(Text(expr, total = false))
        case (None, IsTotal(TotalYes)) => Right(Text(Constant(""), total = true))
        case (None, IsTotal(TotalNo)) => Right(Text(Constant(""), total = false))
        case (Some(invalidValue), invalidTotal) => Left(
          InvalidState(s"""|Unsupported type of value for text field
                           |Id: $id
                           |Value: $invalidValue
                           |Total: $invalidTotal""".stripMargin)
        )
      }
    case Some(DateRaw) =>
      val finalOffset = offset.getOrElse(Offset(0))

      val valueOpt =
        value match {
          case Some(DateExpression(dateExpr)) => Right(Some(dateExpr))
          case None => Right(None)
          case Some(invalidValue) => Left(
            InvalidState(s"""|Unsupported type of value for date field
                             |Id: $id
                             |Value: $invalidValue""".stripMargin)
          )
        }

      val formatOpt =
        format match {
          case Some(DateFormat(format)) => Right(format)
          case None => Right(AnyDate)
          case Some(invalidFormat) => Left(
            InvalidState(s"""|Unsupported type of format for date field
                             |Id: $id
                             |Format: $invalidFormat""".stripMargin)
          )
        }

      for {
        value <- valueOpt
        format <- formatOpt
      } yield Date(format, finalOffset, value)
    case Some(AddressRaw) => Right(Address)
    case Some(ChoiceRaw) =>
      (format, choices, multivalue, value) match {
        case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) =>
          Right(Choice(Checkbox, NonEmptyList(x, xs), Vertical, selections))
        case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo), Selections(selections)) =>
          Right(Choice(Radio, NonEmptyList(x, xs), Vertical, selections))
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) =>
          Right(Choice(Checkbox, NonEmptyList(x, xs), Horizontal, selections))
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo), Selections(selections)) =>
          Right(Choice(Radio, NonEmptyList(x, xs), Horizontal, selections))
        case (IsOrientation(YesNoOrientation), None, IsMultivalue(MultivalueNo), Selections(selections)) =>
          Right(Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, selections))
        case (invalidFormat, invalidChoices, invalidMultivalue, invalidValue) => Left(
          InvalidState(s"""|Unsupported combination of 'format, choices, multivalue and value':
                           |Format     : $invalidFormat
                           |Choices    : $invalidChoices
                           |Multivalue : $invalidMultivalue
                           |Value      : $invalidValue
                           |""".stripMargin)
        )
      }
  }

  private final object Selections {
    def unapply(choiceExpr: Option[ValueExpr]): Option[List[Int]] = {
      choiceExpr match {
        case Some(ChoiceExpression(selections)) => Some(selections)
        case None => Some(List.empty[Int])
        case Some(_) => None
      }
    }
  }

  private sealed trait OrientationValue

  private final case object VerticalOrientation extends OrientationValue
  private final case object HorizontalOrientation extends OrientationValue
  private final case object YesNoOrientation extends OrientationValue

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[OrientationValue] = {
      orientation match {
        case Some(TextFormat("vertical")) | None => Some(VerticalOrientation)
        case Some(TextFormat("horizontal")) => Some(HorizontalOrientation)
        case Some(TextFormat("yesno")) => Some(YesNoOrientation)
        case _ => None
      }
    }
  }

  private sealed trait Total

  private final case object TotalYes extends Total
  private final case object TotalNo extends Total

  private final object IsTotal {
    def unapply(total: Option[String]): Option[Total] = {
      total match {
        case Some(IsFalseish()) | None => Some(TotalNo)
        case Some(IsTrueish()) => Some(TotalYes)
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
