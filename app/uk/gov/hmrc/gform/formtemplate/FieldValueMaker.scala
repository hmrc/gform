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

package uk.gov.hmrc.gform.formtemplate

import cats.data.NonEmptyList
import cats.syntax.all._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.{ FormatParser, PresentationHintParser, ValueParser }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class MES(mandatory: Boolean, editable: Boolean, submissible: Boolean)

class FieldValueMaker(json: JsValue) {

  lazy val id: FieldId = (json \ "id").as[FieldId]
  lazy val `type`: Option[ComponentTypeRaw] = (json \ "type").asOpt[ComponentTypeRaw]
  lazy val label: String = (json \ "label").as[String]

  lazy val optMaybeValueExpr: Opt[Option[ValueExpr]] = parse("value", ValueParser.validate)
  lazy val optMaybeFormatExpr: Opt[Option[FormatExpr]] = parse("format", FormatParser.validate)
  lazy val optMaybePresentationHintExpr: Opt[Option[List[PresentationHint]]] = parse("presentationHint", PresentationHintParser.validate)

  lazy val helpText: Option[String] = (json \ "helpText").asOpt[String]
  lazy val optionHelpText: Option[List[String]] = (json \ "optionHelpText").asOpt[List[String]]
  lazy val submitMode: Option[String] = (json \ "submitMode").asOpt[String]
  lazy val choices: Option[List[String]] = (json \ "choices").asOpt[List[String]]

  lazy val fieldsJson: Option[List[JsValue]] = (json \ "fields").asOpt[List[JsValue]]
  lazy val errorMessage: Option[String] = (json \ "errorMessage").asOpt[String]

  lazy val fields: Option[List[FieldValueMaker]] = fieldsJson.map(_.map(new FieldValueMaker(_)))

  lazy val mandatory: Option[String] = (json \ "mandatory").asOpt[String]
  lazy val multivalue: Option[String] = (json \ "multivalue").asOpt[String]
  lazy val total: Option[String] = (json \ "total").asOpt[String]
  lazy val international: Option[String] = (json \ "international").asOpt[String]
  lazy val infoText: Option[String] = (json \ "infoText").asOpt[String]
  lazy val infoType: Option[String] = (json \ "infoType").asOpt[String]
  lazy val shortName: Option[String] = (json \ "shortName").asOpt[String]
  lazy val optMaybeRepeatsMax: Opt[Option[Int]] = toOpt((json \ "repeatsMax").validateOpt[Int])
  lazy val optMaybeRepeatsMin: Opt[Option[Int]] = toOpt((json \ "repeatsMin").validateOpt[Int])
  lazy val repeatLabel: Option[String] = (json \ "repeatLabel").asOpt[String]
  lazy val repeatAddAnotherText: Option[String] = (json \ "repeatAddAnotherText").asOpt[String]

  def optFieldValue(): Opt[FieldValue] =
    for {
      presHint <- optMaybePresentationHintExpr
      mes <- optMES
      ct <- componentTypeOpt
    } yield mkFieldValue(presHint, mes, ct)

  private def toOpt[A](result: JsResult[A]): Opt[A] = {
    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) => UnexpectedState(errors.map {
        case (path, validationErrors) =>
          s"Path: ${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
      }.mkString(",")).asLeft
    }
  }

  private def mkFieldValue(presHint: Option[List[PresentationHint]], mes: MES, ct: ComponentType): FieldValue = FieldValue(
    id = id,
    `type` = ct,
    label = label,
    helpText = helpText,
    shortName = shortName,
    mandatory = mes.mandatory,
    editable = mes.editable,
    submissible = mes.submissible,
    presentationHint = presHint,
    errorMessage = errorMessage
  )

  private lazy val optMES: Opt[MES] = (submitMode, mandatory) match {
    //format: OFF
    case IsThisAnInfoField()                                    => MES(mandatory = true,  editable = false, submissible = false).asRight
    case (Some(IsStandard()) | None, Some(IsTrueish()) | None)  => MES(mandatory = true,  editable = true,  submissible = true).asRight
    case (Some(IsReadOnly()),        Some(IsTrueish()) | None)  => MES(mandatory = true,  editable = false, submissible = true).asRight
    case (Some(IsInfo()),            Some(IsTrueish()) | None)  => MES(mandatory = true,  editable = false, submissible = false).asRight
    case (Some(IsStandard()) | None, Some(IsFalseish()))        => MES(mandatory = false, editable = true,  submissible = true).asRight
    case (Some(IsInfo()),            Some(IsFalseish()))        => MES(mandatory = false, editable = false, submissible = false).asRight
    case otherwise                                              => UnexpectedState(s"Expected 'standard', 'readonly' or 'info' string or nothing for submitMode and expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise").asLeft
    //format: ON
  }

  private lazy val componentTypeOpt: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None => textOpt
    case Some(DateRaw) => dateOpt
    case Some(AddressRaw) => addressOpt
    case Some(GroupRaw) => groupOpt
    case Some(ChoiceRaw) => choiceOpt
    case Some(FileUploadRaw) => fileUploadOpt
    case Some(InfoRaw) => infoOpt
    //TODO: What if there is None
  }

  private lazy val textOpt: Opt[ComponentType] = {
    for {
      maybeFormatExpr <- optMaybeFormatExpr
      maybeValueExpr <- optMaybeValueExpr
      optText = (maybeFormatExpr, maybeValueExpr, total) match {
        //format: OFF
        case (Some(TextFormat(UkSortCodeFormat)), Some(TextExpression(expr)), _) => UkSortCode(expr).asRight
        case (Some(TextFormat(UkSortCodeFormat)), None, _) => UkSortCode(Constant("")).asRight
        case (Some(TextFormat(f)), Some(TextExpression(expr)), IsTotal(TotalYes))  => Text(f, expr, total = true).asRight
        case (Some(TextFormat(f)), Some(TextExpression(expr)), IsTotal(TotalNo))   => Text(f, expr, total = false).asRight
        case (Some(TextFormat(f)), None,                       IsTotal(TotalYes))  => Text(f, Constant(""), total = true).asRight
        case (Some(TextFormat(f)), None,                       IsTotal(TotalNo))   => Text(f, Constant(""), total = false).asRight
        case (None,                Some(TextExpression(expr)), IsTotal(TotalYes))  => Text(ShortText, expr, total = true).asRight
        case (None,                Some(TextExpression(expr)), IsTotal(TotalNo))   => Text(ShortText, expr, total = false).asRight
        case (None,                None,                       IsTotal(TotalYes))  => Text(ShortText, Constant(""), total = true).asRight
        case (None,                None,                       IsTotal(TotalNo))   => Text(ShortText, Constant(""), total = false).asRight
        case (Some(invalidFormat), Some(invalidValue),         invalidTotal)       => UnexpectedState(
          s"""|Unsupported type of format and value for text field
              |Id: $id
              |Format: $invalidFormat
              |Value: $invalidValue
              |Total: $invalidTotal""".stripMargin).asLeft
        //format: ON
      }
      result <- optText
    } yield result
  }

  private lazy val addressOpt: Opt[Address] = international match {
    //format: OFF
    case IsInternational(InternationalYes) => Address(international = true).asRight
    case IsInternational(InternationalNo)  => Address(international = false).asRight
    case invalidInternational       => UnexpectedState(
      s"""|Unsupported type of value for address field
          |Id: $id
          |Total: $invalidInternational""".stripMargin).asLeft
    //format: ON
  }

  private lazy val dateOpt: Opt[Date] = {

    lazy val dateConstraintOpt: Opt[DateConstraintType] =
      for {
        maybeFormatExpr <- optMaybeFormatExpr
        optDateConstraintType = maybeFormatExpr match {
          case Some(DateFormat(e)) => e.asRight
          case None => AnyDate.asRight
          case Some(invalidFormat) =>
            UnexpectedState(
              s"""|Unsupported type of format for date field
                  |Id: $id
                  |Format: $invalidFormat""".stripMargin
            ).asLeft
        }
        dateConstraintType <- optDateConstraintType
      } yield dateConstraintType

    lazy val dateValueOpt: Opt[Option[DateValue]] = {

      for {
        maybeValueExpr <- optMaybeValueExpr
        optMaybeDateValue = maybeValueExpr match {
          case Some(DateExpression(dateExpr)) => dateExpr.some.asRight
          case None => none.asRight
          case Some(invalidValue) => UnexpectedState(
            s"""|Unsupported type of value for date field
                |Id: $id
                |Value: $invalidValue""".stripMargin
          ).asLeft
        }
        maybeDateValue <- optMaybeDateValue
      } yield maybeDateValue

    }

    for {
      maybeDateValue <- dateValueOpt
      dateConstraintType <- dateConstraintOpt
      o = Offset(0)
    } yield Date(dateConstraintType, o, maybeDateValue)
  }

  private lazy val groupOpt: Opt[Group] = fields.fold(noRawFields)(groupOpt(_))

  private lazy val noRawFields: Opt[Group] = UnexpectedState(s"""Require 'fields' element in Group""").asLeft

  def groupOpt(fields: List[FieldValueMaker]): Opt[Group] = {

    def orientation(format: Option[FormatExpr]) = format match {
      case IsGroupOrientation(VerticalGroupOrientation) | None => Vertical
      case IsGroupOrientation(HorizontalGroupOrientation) => Horizontal
    }

    val fieldValueOpts: List[Opt[FieldValue]] = fields.map(_.optFieldValue())

    val fieldValuesOpt: Opt[List[FieldValue]] = {
      import cats.implicits._
      fieldValueOpts.sequence
    }

    for {
      fieldValues <- fieldValuesOpt.right
      format <- optMaybeFormatExpr.right
      repMax <- optMaybeRepeatsMax
      repMin <- optMaybeRepeatsMin
      group <- validateRepeatsAndBuildGroup(repMax, repMin, fieldValues, orientation(format))
    } yield group
  }

  private def validateRepeatsAndBuildGroup(repMax: Option[Int], repMin: Option[Int], fields: List[FieldValue], orientation: Orientation) = {
    (repMax, repMin) match {
      case (Some(repMax), Some(repMin)) if repMax < repMin =>
        UnexpectedState(s"""repeatsMax should be higher than repeatsMin in Group field""").asLeft
      case (Some(repMax), Some(repMin)) if repMin < 1 =>
        UnexpectedState(s"""repeatsMin in Group field cannot be less than 1""").asLeft
      case _ =>
        Group(fields, orientation, repMax, repMin, repeatLabel, repeatAddAnotherText).asRight
    }
  }

  private lazy val choiceOpt: Opt[Choice] = {
    for {
      maybeFormatExpr <- optMaybeFormatExpr
      maybeValueExpr <- optMaybeValueExpr
      oChoice: Opt[Choice] = (maybeFormatExpr, choices, multivalue, maybeValueExpr, optionHelpText) match {
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
          UnexpectedState(s"""|Unsupported combination of 'format, choices, multivalue and value':
                           |Format     : $invalidFormat
                           |Choices    : $invalidChoices
                           |Multivalue : $invalidMultivalue
                           |Value      : $invalidValue
                           |optionHelpText: $invalidHelpText
                           |""".stripMargin).asLeft
      }
      result <- oChoice.right
    } yield result
  }

  private lazy val fileUploadOpt: Opt[FileUpload] = FileUpload().asRight

  private lazy val infoOpt: Opt[InformationMessage] = (infoType, infoText) match {
    case (IsInfoType(StandardInfo), Some(infText)) => InformationMessage(StandardInfo, infText).asRight
    case (IsInfoType(LongInfo), Some(infText)) => InformationMessage(LongInfo, infText).asRight
    case (IsInfoType(ImportantInfo), Some(infText)) => InformationMessage(ImportantInfo, infText).asRight
    case (IsInfoType(BannerInfo), Some(infText)) => InformationMessage(BannerInfo, infText).asRight
    case (IsInfoType(NoFormat), Some(infText)) => InformationMessage(NoFormat, infText).asRight
    case (infType, infText) => UnexpectedState(
      s"""
         | Invalid or missing arguments in 'info' field. The 'info' field should contain the infoType and
         | infoText arguments. infoType is one of: standard, long, important or banner.
         | infoText is the text to display.
         | InfoType: $infType
         | InfoText: $infText
       """.stripMargin
    ).asLeft
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

  private sealed trait ChoiceOrientation

  private final case object VerticalOrientation extends ChoiceOrientation
  private final case object HorizontalOrientation extends ChoiceOrientation
  private final case object YesNoOrientation extends ChoiceOrientation
  private final case object InlineOrientation extends ChoiceOrientation

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[ChoiceOrientation] = {
      orientation match {
        case Some(OrientationFormat("vertical")) | None => Some(VerticalOrientation)
        case Some(OrientationFormat("horizontal")) => Some(HorizontalOrientation)
        case Some(OrientationFormat("yesno")) => Some(YesNoOrientation)
        case Some(OrientationFormat("inline")) => Some(InlineOrientation)

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
        case Some(OrientationFormat("vertical")) | None => Some(VerticalGroupOrientation)
        case Some(OrientationFormat("horizontal")) => Some(HorizontalGroupOrientation)
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
        case Some(IsTrueish()) => Some(

          MultivalueYes
        )
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
    def unapply(maybeStandard: String): Boolean =

      maybeStandard.toLowerCase == "standard"
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

  private final object IsInfoType {
    def unapply(arg: Option[String]): Option[InfoType] = {
      arg match {
        case Some(infoTypeString) => infoTypeString.toLowerCase match {
          case "standard" => Some(StandardInfo)
          case "long" => Some(LongInfo)
          case "important" => Some(ImportantInfo)
          case "banner" => Some(BannerInfo)
          case "noformat" => Some(NoFormat)
          case _ => None
        }
        case None => Some(StandardInfo)
      }
    }
  }

  private final object IsThisAnInfoField {
    def unapply(ignoredArgs: (Option[String], Option[String])) = `type`.getOrElse(None).isInstanceOf[InfoRaw.type]
  }

  private def parse[T: Reads, R](path: String, validate: T => Opt[R]): Opt[Option[R]] = {
    val optMaybeString: Opt[Option[T]] = toOpt((json \ path).
      validateOpt[T])
    import cats.implicits._
    for {
      maybeString <- optMaybeString.right
      res <- maybeString.map(validate).sequenceU
    } yield res
  }
}

