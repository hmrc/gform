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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.syntax.all._
import play.api.libs.json.{ OFormat, _ }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.{ InvalidState, UnexpectedState }
import FieldValueRaw._
import play.api.libs.functional.syntax._

object FieldValueRaw {

  implicit val format: OFormat[FieldValueRaw] = (
    (__ \ 'id).format[FieldId] and
    (__ \ 'type).formatNullable[ComponentTypeRaw] and
    (__ \ 'label).format[String] and
    (__ \ 'value).formatNullable[ValueExpr] and
    (__ \ 'format).formatNullable[FormatExpr] and
    (__ \ 'helpText).formatNullable[String] and
    (__ \ 'optionHelpText).formatNullable[List[String]] and
    (__ \ 'submitMode).formatNullable[String] and
    (__ \ 'choices).formatNullable[List[String]] and
    (__ \ 'fields).lazyFormatNullable(implicitly[Format[List[FieldValueRaw]]]) and //Note: recursiveness here prevents macro use
    (__ \ 'mandatory).formatNullable[String] and
    (__ \ 'offset).formatNullable[Offset] and
    (__ \ 'multivalue).formatNullable[String] and
    (__ \ 'total).formatNullable[String]
  )(FieldValueRaw.apply, unlift(FieldValueRaw.unapply))

  case class MES(mandatory: Boolean, editable: Boolean, submissible: Boolean)
}

case class FieldValueRaw(
    id: FieldId,
    `type`: Option[ComponentTypeRaw] = None,
    label: String,
    value: Option[ValueExpr] = None,
    format: Option[FormatExpr] = None,
    helpText: Option[String] = None,
    optionHelpText: Option[List[String]] = None,
    submitMode: Option[String] = None,
    choices: Option[List[String]] = None,
    fields: Option[List[FieldValueRaw]] = None,
    mandatory: Option[String] = None,
    offset: Option[Offset] = None,
    multivalue: Option[String] = None,
    total: Option[String] = None,
    international: Option[String] = None
) {

  def toFieldValue = Reads[FieldValue] { _ => getFieldValue fold (us => JsError(us.toString), fv => JsSuccess(fv)) }

  private def getFieldValue(): Opt[FieldValue] = optMES.flatMap(mes => componentTypeOpt.map(ct => mkFieldValue(mes, ct)))

  private def mkFieldValue(mes: MES, ct: ComponentType): FieldValue = FieldValue(
    id = id,
    `type` = ct,
    label = label,
    helpText = helpText,
    mandatory = mes.mandatory,
    editable = mes.editable,
    submissible = mes.submissible
  )

  private lazy val optMES: Opt[MES] = (submitMode, mandatory) match {
    //format: OFF
    case (Some(IsStandard()) | None, Some(IsTrueish()) | None)  => MES(mandatory = true, editable = true, submissible = true).asRight
    case (Some(IsReadOnly()),        Some(IsTrueish()) | None)  => MES(mandatory = true, editable = false, submissible = true).asRight
    case (Some(IsInfo()),            Some(IsTrueish()) | None)  => MES(mandatory = true, editable = false, submissible = false).asRight
    case (Some(IsStandard()) | None, Some(IsFalseish()))        => MES(mandatory = false, editable = true, submissible = true).asRight
    case (Some(IsInfo()),            Some(IsFalseish()))        => MES(mandatory = false, editable = false, submissible = false).asRight
    case otherwise                                              => InvalidState(s"Expected 'standard', 'readonly' or 'info' string or nothing for submitMode and expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise").asLeft
    //format: ON
  }

  private lazy val valueOpt: Opt[Option[DateValue]] = value match {
    case Some(DateExpression(dateExpr)) => dateExpr.some.asRight
    case None => none.asRight
    case Some(invalidValue) => InvalidState(
      s"""|Unsupported type of value for date field
          |Id: $id
          |Value: $invalidValue""".stripMargin
    ).asLeft
  }

  private lazy val componentTypeOpt: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None => textOpt
    case Some(DateRaw) => dateOpt
    case Some(AddressRaw) => addressOpt
    case Some(AddressRaw) => addressOpt
    case Some(GroupRaw) => groupOpt
    case Some(ChoiceRaw) => choiceOpt
    case Some(FileUploadRaw) => fileUploadOpt
    //TODO: What if there is None
  }

  private lazy val textOpt: Opt[Text] = (value, total) match {
    //format: OFF
    case (Some(TextExpression(expr)), IsTotal(TotalYes))  => Text(expr, total = true).asRight
    case (Some(TextExpression(expr)), IsTotal(TotalNo))   => Text(expr, total = false).asRight
    case (None,                       IsTotal(TotalYes))  => Text(Constant(""), total = true).asRight
    case (None,                       IsTotal(TotalNo))   => Text(Constant(""), total = false).asRight
    case (Some(invalidValue),         invalidTotal)       => InvalidState(
      s"""|Unsupported type of value for text field
          |Id: $id
          |Value: $invalidValue
          |Total: $invalidTotal""".stripMargin).asLeft
    //format: ON
  }

  private lazy val addressOpt: Opt[Address] = international match {
    //format: OFF
    case IsInternational(InternationalYes) => Address(international = true).asRight
    case IsInternational(InternationalNo)  => Address(international = false).asRight
    case invalidInternational       => InvalidState(
      s"""|Unsupported type of value for address field
          |Id: $id
          |Total: $invalidInternational""".stripMargin).asLeft
    //format: ON
  }

  private lazy val formatOpt: Opt[DateConstraintType] = format match {
    case Some(DateFormat(e)) => e.asRight
    case None => AnyDate.asRight
    case Some(invalidFormat) =>
      InvalidState(s"""|Unsupported type of format for date field
                       |Id: $id
                       |Format: $invalidFormat""".stripMargin).asLeft
  }

  private lazy val dateOpt: Opt[Date] = for {
    v <- valueOpt
    f <- formatOpt
    o = offset.getOrElse(Offset(0))
  } yield Date(f, o, v)

  private lazy val groupOpt: Opt[Group] = fields.fold(noRawFields)(groupOpt(_))
  private lazy val noRawFields: Opt[Group] = InvalidState(s"""Require 'fields' element in Group""").asLeft
  private def groupOpt(rawFields: List[FieldValueRaw]): Opt[Group] = {

    val orientation = format match {
      case IsGroupOrientation(VerticalGroupOrientation) | None => Vertical
      case IsGroupOrientation(HorizontalGroupOrientation) => Horizontal
    }

    rawFields.map(_.getFieldValue()).partition(_.isRight) match {
      case (ueorfvs, Nil) => Group(ueorfvs.map(_.right.get), orientation).asRight
      case (_, ueorfvs) => ueorfvs.map(_.left.get).head.asLeft
    }
  }

  private lazy val choiceOpt = (format, choices, multivalue, value, optionHelpText) match {
    case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections), oHelpText) =>
      Choice(Checkbox, NonEmptyList(x, xs), Vertical, selections, oHelpText).asRight
    case (IsOrientation(VerticalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo), Selections(selections), oHelpText) =>
      Choice(Radio, NonEmptyList(x, xs), Vertical, selections, oHelpText).asRight
    case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections), oHelpText) =>
      Choice(Checkbox, NonEmptyList(x, xs), Horizontal, selections, oHelpText).asRight
    case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo), Selections(selections), oHelpText) =>
      Choice(Radio, NonEmptyList(x, xs), Horizontal, selections, oHelpText).asRight
    case (IsOrientation(YesNoOrientation), None, IsMultivalue(MultivalueNo), Selections(selections), oHelpText) =>
      Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, selections, oHelpText).asRight
    case (IsOrientation(YesNoOrientation), _, _, Selections(selections), oHelpText) =>
      Choice(YesNo, NonEmptyList.of("Yes", "No"), Horizontal, selections, oHelpText).asRight
    case (IsOrientation(InlineOrientation), Some(x :: xs), None, Selections(selections), oHelpText) =>
      Choice(Inline, NonEmptyList(x, xs), Horizontal, selections, oHelpText).asRight
    case (invalidFormat, invalidChoices, invalidMultivalue, invalidValue, invalidHelpText) =>
      InvalidState(s"""|Unsupported combination of 'format, choices, multivalue and value':
                         |Format     : $invalidFormat
                         |Choices    : $invalidChoices
                         |Multivalue : $invalidMultivalue
                         |Value      : $invalidValue
                         |optionHelpText: $invalidHelpText
                         |""".stripMargin).asLeft
  }

  private lazy val fileUploadOpt: Opt[FileUpload] = FileUpload().asRight

  private final object Selections {
    def unapply(choiceExpr: Option[ValueExpr]): Option[List[Int]] = {
      choiceExpr match {
        case Some(ChoiceExpression(selections)) => Some(selections)
        case None => Some(List.empty[Int])
        case Some(_) => None
      }
    }
  }

  private sealed trait ChoiceOrientation

  private final case object VerticalOrientation extends ChoiceOrientation
  private final case object HorizontalOrientation extends ChoiceOrientation
  private final case object YesNoOrientation extends ChoiceOrientation
  private final case object InlineOrientation extends ChoiceOrientation

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[ChoiceOrientation] = {
      orientation match {
        case Some(TextFormat("vertical")) | None => Some(VerticalOrientation)
        case Some(TextFormat("horizontal")) => Some(HorizontalOrientation)
        case Some(TextFormat("yesno")) => Some(YesNoOrientation)
        case Some(TextFormat("inline")) => Some(InlineOrientation)
        case _ => None
      }
    }
  }

  private sealed trait GroupOrientation

  private final case object VerticalGroupOrientation extends GroupOrientation
  private final case object HorizontalGroupOrientation extends GroupOrientation

  private final object IsGroupOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[GroupOrientation] = {
      orientation match {
        case Some(TextFormat("vertical")) | None => Some(VerticalGroupOrientation)
        case Some(TextFormat("horizontal")) => Some(HorizontalGroupOrientation)
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

  private sealed trait International

  private final case object InternationalYes extends International
  private final case object InternationalNo extends International

  private final object IsInternational {
    def unapply(international: Option[String]): Option[International] = {
      international match {
        case Some(IsFalseish()) | None => Some(InternationalNo)
        case Some(IsTrueish()) => Some(InternationalYes)
        case _ => None
      }
    }
  }

  // Should we instead do this with an case insensitive extractor
  object IsStandard {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "standard"
  }
  object IsReadOnly {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "readonly"
  }
  object IsInfo {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "info"
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
