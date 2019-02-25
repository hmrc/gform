/*
 * Copyright 2019 HM Revenue & Customs
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
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.{ FormatParser, PresentationHintParser, ValueParser }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.RoundingMode._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class MES(
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false)

class FormComponentMaker(json: JsValue) {

  lazy val id: FormComponentId = (json \ "id").as[FormComponentId]
  lazy val `type`: Option[ComponentTypeRaw] = (json \ "type").asOpt[ComponentTypeRaw]
  lazy val label: String = (json \ "label").as[String]

  lazy val optMaybeValueExpr: Opt[Option[ValueExpr]] = parse("value", ValueParser.validate)
  lazy val optMaybeFormatExpr: RoundingMode => Opt[Option[FormatExpr]] = rm =>
    parse("format", FormatParser.validate(rm))
  lazy val optMaybePresentationHintExpr: Opt[Option[List[PresentationHint]]] =
    parse("presentationHint", PresentationHintParser.validate)

  lazy val helpText: Option[String] = (json \ "helpText").asOpt[String]
  lazy val optionHelpText: Option[List[String]] = (json \ "optionHelpText").asOpt[List[String]]
  lazy val submitMode: Option[String] = (json \ "submitMode").asOpt[String]
  lazy val choices: Option[List[String]] = (json \ "choices").asOpt[List[String]]

  lazy val idType: Option[String] = (json \ "idType").asOpt[String]
  lazy val idNumber: Opt[Option[ValueExpr]] = parse("idNumber", ValueParser.validate)
  lazy val regimeType: Option[String] = (json \ "regimeType").asOpt[String]

  lazy val fieldsJson: Option[List[JsValue]] = (json \ "fields").asOpt[List[JsValue]]
  lazy val errorMessage: Option[String] = (json \ "errorMessage").asOpt[String]

  lazy val fields: Option[List[FormComponentMaker]] = fieldsJson.map(_.map(new FormComponentMaker(_)))
  lazy val validIf: Option[ValidIf] = (json \ "validIf").asOpt[ValidIf]
  lazy val mandatory: Option[String] = (json \ "mandatory").asOpt[String]
  lazy val multiline: Option[String] = (json \ "multiline").asOpt[String]
  lazy val displayWidth: Option[String] = (json \ "displayWidth").asOpt[String]
  lazy val roundingMode: RoundingMode = (json \ "round").asOpt[RoundingMode].getOrElse(RoundingMode.defaultRoundingMode)
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

  def optFieldValue(): Opt[FormComponent] =
    for {
      presHint <- optMaybePresentationHintExpr
      mes      <- optMES
      ct       <- componentTypeOpt
    } yield mkFieldValue(presHint, mes, ct)

  private def toOpt[A](result: JsResult[A]): Opt[A] =
    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) =>
        UnexpectedState(
          errors
            .map {
              case (path, validationErrors) =>
                s"Path: ${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
            }
            .mkString(",")).asLeft
    }

  private def mkFieldValue(presHint: Option[List[PresentationHint]], mes: MES, ct: ComponentType): FormComponent =
    FormComponent(
      id = id,
      `type` = ct,
      label = label,
      helpText = helpText,
      shortName = shortName,
      validIf = validIf,
      mandatory = mes.mandatory,
      editable = mes.editable,
      submissible = mes.submissible,
      derived = mes.derived,
      onlyShowOnSummary = mes.onlyShowOnSummary,
      presentationHint = presHint,
      errorMessage = errorMessage
    )

  private lazy val optMES: Opt[MES] = (submitMode, mandatory, optMaybeValueExpr) match {
    // format: off
    case IsThisAnInfoField()                                                     => MES(mandatory = true,  editable = false, submissible = false, derived = false).asRight
    case (Some(IsStandard()) | None, Some(IsTrueish())  | None,  _)              => MES(mandatory = true,  editable = true,  submissible = true,  derived = false).asRight
    case (Some(IsReadOnly()),        Some(IsTrueish())  | None,  _)              => MES(mandatory = true,  editable = false, submissible = true,  derived = false).asRight
    case (Some(IsInfo()),            Some(IsTrueish())  | None,  _)              => MES(mandatory = true,  editable = false, submissible = false, derived = false).asRight
    case (Some(IsStandard()) | None, Some(IsFalseish()),         _)              => MES(mandatory = false, editable = true,  submissible = true,  derived = false).asRight
    case (Some(IsInfo()),            Some(IsFalseish()),         _)              => MES(mandatory = false, editable = false, submissible = false, derived = false).asRight
    case (Some(IsDerived()),         Some(IsTrueish())  | None,  Right(Some(_))) => MES(mandatory = true,  editable = false, submissible = true,  derived = true).asRight
    case (Some(IsDerived()),         Some(IsFalseish()),         Right(Some(_))) => MES(mandatory = false, editable = false, submissible = true,  derived = true).asRight
    case (Some(IsNonSubmissible()),  Some(IsFalseish()) | None,  _)              => MES(mandatory = false, editable = true,  submissible = false, derived = false).asRight
    case (Some(IsNonSubmissible()),  Some(IsTrueish())  | None,  _)              => MES(mandatory = true,  editable = true,  submissible = false, derived = false).asRight
    case (Some(IsSummaryInfoOnly()), Some(IsTrueish())  | Some(IsFalseish()) | None, Right(Some(_))) =>
                                                                                    MES(mandatory = true,  editable = false, submissible = false, derived = false, onlyShowOnSummary = true).asRight
    case otherwise =>
      UnexpectedState(
        s"Expected 'standard', summaryinfoonly,'notsubmitted', readonly' or 'info' string or nothing for submitMode and expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise").asLeft
    // format: on
  }

  private lazy val componentTypeOpt: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None   => textOpt
    case Some(DateRaw)          => dateOpt
    case Some(AddressRaw)       => addressOpt
    case Some(GroupRaw)         => groupOpt
    case Some(ChoiceRaw)        => choiceOpt
    case Some(FileUploadRaw)    => fileUploadOpt
    case Some(InfoRaw)          => infoOpt
    case Some(HmrcTaxPeriodRaw) => hmrcTaxPeriodOpt
    //TODO: What if there is None
  }

  private lazy val textOpt: Opt[ComponentType] = {
    for {
      maybeFormatExpr <- optMaybeFormatExpr(roundingMode)
      maybeValueExpr  <- optMaybeValueExpr
      result <- (maybeFormatExpr, maybeValueExpr, multiline, displayWidth) match {
                 // format: off
        case (Some(TextFormat(UkSortCodeFormat)), HasTextExpression(expr), IsNotMultiline(), _)                       => UkSortCode(expr).asRight
        case (Some(TextFormat(f)),                HasTextExpression(expr), IsNotMultiline(), None)                    => Text(f, expr).asRight
        case (None,                               HasTextExpression(expr), IsNotMultiline(), None)                    => Text(ShortText, expr).asRight
        case (Some(TextFormat(f)),                HasTextExpression(expr), IsNotMultiline(), HasDisplayWidth(dw))     => Text(f, expr, dw).asRight
        case (None,                               HasTextExpression(expr), IsNotMultiline(), HasDisplayWidth(dw))     => Text(ShortText, expr, dw).asRight
        case (Some(TextFormat(f)),                HasTextExpression(expr), IsNotMultiline(), HasDisplayWidth(dw))     => Text(f, expr, dw).asRight
        case (None,                               HasTextExpression(expr), IsNotMultiline(), HasDisplayWidth(dw))     => Text(ShortText, expr, dw).asRight
        case (Some(TextFormat(f)),                HasTextExpression(expr), IsMultiline()   , None)                    => TextArea(f, expr).asRight
        case (None,                               HasTextExpression(expr), IsMultiline()   , None)                    => TextArea(BasicText, expr).asRight
        case (Some(TextFormat(f)),                HasTextExpression(expr), IsMultiline()   , HasDisplayWidth(dw))     => TextArea(f, expr,dw).asRight
        case (None,                               HasTextExpression(expr), IsMultiline()   , HasDisplayWidth(dw))     => TextArea(BasicText, expr,dw).asRight
        case (maybeInvalidFormat,                 maybeInvalidValue,       IsMultiline()   , _) =>
          UnexpectedState(s"""|Unsupported type of format or value for multiline text field
                  |Id: $id
                  |Format: $maybeInvalidFormat
                  |Value: $maybeInvalidValue
                  |""".stripMargin).asLeft
        case (Some(invalidFormat),                None,                    IsNotMultiline(), _) =>
          UnexpectedState(s"""|Unsupported type of format and value for text field
                  |Id: $id
                  |Format: $invalidFormat
                  |Value: must supply a value
                  |""".stripMargin).asLeft
        case (None,                               Some(invalidValue),      IsNotMultiline(), _) =>
          UnexpectedState(s"""|Unsupported type of format and value for text field
                  |Id: $id
                  |Format: "must supply a value for format"
                  |Value: $invalidValue
                  |""".stripMargin).asLeft
        case (Some(invalidFormat),                Some(invalidValue),      IsNotMultiline(), _) =>
          UnexpectedState(s"""|Unsupported type of format and value for text field
                  |Id: $id
                  |Format: $invalidFormat
                  |Value: $invalidValue
                  |""".stripMargin).asLeft
      }
      // format: on
    } yield result
  }

  private final object HasDisplayWidth {
    def unapply(displayWidth: Option[String]): Option[DisplayWidth] =
      displayWidth match {
        case Some("xs")  => Some(DisplayWidth.XS)
        case Some("s")   => Some(DisplayWidth.S)
        case Some("m")   => Some(DisplayWidth.M)
        case Some("l")   => Some(DisplayWidth.L)
        case Some("xl")  => Some(DisplayWidth.XL)
        case Some("xxl") => Some(DisplayWidth.XXL)
        case _           => Some(DisplayWidth.DEFAULT)
      }
  }

  private lazy val addressOpt: Opt[Address] = international match {
    case IsInternational(InternationalYes) => Address(international = true).asRight
    case IsInternational(InternationalNo)  => Address(international = false).asRight
    case invalidInternational =>
      UnexpectedState(s"""|Unsupported type of value for address field
                          |Id: $id
                          |Total: $invalidInternational""".stripMargin).asLeft
  }

  private lazy val dateOpt: Opt[Date] = {

    lazy val dateConstraintOpt: Opt[DateConstraintType] =
      for {
        maybeFormatExpr <- optMaybeFormatExpr(roundingMode)
        optDateConstraintType = maybeFormatExpr match {
          case Some(DateFormat(e)) => e.asRight
          case None                => AnyDate.asRight
          case Some(invalidFormat) =>
            UnexpectedState(s"""|Unsupported type of format for date field
                                |Id: $id
                                |Format: $invalidFormat""".stripMargin).asLeft
        }
        dateConstraintType <- optDateConstraintType
      } yield dateConstraintType

    lazy val dateValueOpt: Opt[Option[DateValue]] = {

      for {
        maybeValueExpr <- optMaybeValueExpr
        optMaybeDateValue = maybeValueExpr match {
          case Some(DateExpression(dateExpr)) => dateExpr.some.asRight
          case None                           => none.asRight
          case Some(invalidValue) =>
            UnexpectedState(s"""|Unsupported type of value for date field
                                |Id: $id
                                |Value: $invalidValue""".stripMargin).asLeft
        }
        maybeDateValue <- optMaybeDateValue
      } yield maybeDateValue

    }

    for {
      maybeDateValue     <- dateValueOpt
      dateConstraintType <- dateConstraintOpt
      o = Offset(0)
    } yield Date(dateConstraintType, o, maybeDateValue)
  }

  private lazy val groupOpt: Opt[Group] = fields.fold(noRawFields)(groupOpt(_))

  private lazy val noRawFields: Opt[Group] = UnexpectedState(s"""Require 'fields' element in Group""").asLeft

  def groupOpt(fields: List[FormComponentMaker]): Opt[Group] = {

    def orientation(format: Option[FormatExpr]) = format match {
      case IsGroupOrientation(VerticalGroupOrientation) | None => Vertical
      case IsGroupOrientation(HorizontalGroupOrientation)      => Horizontal
    }

    val fieldValuesOpt: Opt[List[FormComponent]] = fields.traverse(_.optFieldValue())

    for {
      fieldValues <- fieldValuesOpt.right
      format      <- optMaybeFormatExpr(roundingMode).right
      repMax      <- optMaybeRepeatsMax
      repMin      <- optMaybeRepeatsMin
      group       <- validateRepeatsAndBuildGroup(repMax, repMin, fieldValues, orientation(format))
    } yield group
  }

  private def validateRepeatsAndBuildGroup(
    repMax: Option[Int],
    repMin: Option[Int],
    fields: List[FormComponent],
    orientation: Orientation): Either[UnexpectedState, Group] =
    (repMax, repMin) match {
      case (Some(repMaxV), Some(repMinV)) if repMaxV < repMinV =>
        UnexpectedState(s"""repeatsMax should be higher than repeatsMin in Group field""").asLeft
      case (Some(repMaxV), Some(repMinV)) if repMinV < 0 =>
        UnexpectedState(s"""repeatsMin in Group field cannot be a negative number""").asLeft
      case (_, repeatsMin) =>
        val fieldsMandatory =
          if (repeatsMin.getOrElse(None) == 0) fields.map(field => field.copy(mandatory = false)) else fields
        Group(fieldsMandatory, orientation, repMax, repMin, repeatLabel, repeatAddAnotherText).asRight
    }

  private lazy val choiceOpt: Opt[Choice] = {
    for {
      maybeFormatExpr <- optMaybeFormatExpr(roundingMode)
      maybeValueExpr  <- optMaybeValueExpr
      oChoice: Opt[Choice] = (maybeFormatExpr, choices, multivalue, maybeValueExpr, optionHelpText) match {
        // format: off
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections), oHelpText) => Choice(Checkbox, NonEmptyList(x, xs),          Vertical,   selections, oHelpText).asRight
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections), oHelpText) => Choice(Radio,    NonEmptyList(x, xs),          Vertical,   selections, oHelpText).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections), oHelpText) => Choice(Checkbox, NonEmptyList(x, xs),          Horizontal, selections, oHelpText).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections), oHelpText) => Choice(Radio,    NonEmptyList(x, xs),          Horizontal, selections, oHelpText).asRight
        case (IsOrientation(YesNoOrientation),      None,          IsMultivalue(MultivalueNo),  Selections(selections), oHelpText) => Choice(YesNo,    NonEmptyList.of("Yes", "No"), Horizontal, selections, oHelpText).asRight
        case (IsOrientation(YesNoOrientation),      _,             _,                           Selections(selections), oHelpText) => Choice(YesNo,    NonEmptyList.of("Yes", "No"), Horizontal, selections, oHelpText).asRight
        case (IsOrientation(InlineOrientation),     Some(x :: xs), None,                        Selections(selections), oHelpText) => Choice(Inline,   NonEmptyList(x, xs),          Horizontal, selections, oHelpText).asRight
        // format: on
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

  private lazy val hmrcTaxPeriodOpt: Opt[HmrcTaxPeriod] = {
    val oHmrcTaxPeriod: Opt[HmrcTaxPeriod] = (idType, idNumber, regimeType) match {
      case (Some(a), Right(Some(b: TextExpression)), Some(c)) => HmrcTaxPeriod(IdType(a), b, RegimeType(c)).asRight
    }
    oHmrcTaxPeriod
  }

  private lazy val fileUploadOpt: Opt[FileUpload] = FileUpload().asRight

  private lazy val infoOpt: Opt[InformationMessage] = (infoType, infoText) match {
    case (IsInfoType(StandardInfo), Some(infText))  => InformationMessage(StandardInfo, infText).asRight
    case (IsInfoType(LongInfo), Some(infText))      => InformationMessage(LongInfo, infText).asRight
    case (IsInfoType(ImportantInfo), Some(infText)) => InformationMessage(ImportantInfo, infText).asRight
    case (IsInfoType(BannerInfo), Some(infText))    => InformationMessage(BannerInfo, infText).asRight
    case (IsInfoType(NoFormat), Some(infText))      => InformationMessage(NoFormat, infText).asRight
    case (infType, infText)                         => UnexpectedState(s"""
                                                  | Invalid or missing arguments in 'info' field. The 'info' field should contain the infoType and
                                                  | infoText arguments. infoType is one of: standard, long, important, banner or noformat.
                                                  | infoText is the text to display.
                                                  | InfoType: $infType
                                                  | InfoText: $infText
       """.stripMargin).asLeft
  }

  private final object Selections {
    def unapply(choiceExpr: Option[ValueExpr]): Option[List[Int]] =
      choiceExpr match {
        case Some(ChoiceExpression(selections)) => Some(selections)
        case None                               => Some(List.empty[Int])
        case Some(_)                            => None
      }
  }

  private sealed trait ChoiceOrientation

  private final case object VerticalOrientation extends ChoiceOrientation
  private final case object HorizontalOrientation extends ChoiceOrientation
  private final case object YesNoOrientation extends ChoiceOrientation
  private final case object InlineOrientation extends ChoiceOrientation

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[ChoiceOrientation] =
      orientation match {
        case Some(OrientationFormat("vertical")) | None => Some(VerticalOrientation)
        case Some(OrientationFormat("horizontal"))      => Some(HorizontalOrientation)
        case Some(OrientationFormat("yesno"))           => Some(YesNoOrientation)
        case Some(OrientationFormat("inline"))          => Some(InlineOrientation)

        case _ => None
      }
  }

  private sealed trait GroupOrientation

  private final case object VerticalGroupOrientation extends GroupOrientation
  private final case object HorizontalGroupOrientation extends GroupOrientation

  private final object IsGroupOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[GroupOrientation] =
      orientation match {
        case Some(OrientationFormat("vertical")) | None => Some(VerticalGroupOrientation)
        case Some(OrientationFormat("horizontal"))      => Some(HorizontalGroupOrientation)
        case _                                          => None
      }
  }

  private sealed trait Total

  private final case object TotalYes extends Total
  private final case object TotalNo extends Total

  private final object IsTotal {
    def unapply(total: Option[String]): Option[Total] =
      total match {
        case Some(IsFalseish()) | None => Some(TotalNo)
        case Some(IsTrueish())         => Some(TotalYes)
        case _                         => None
      }
  }

  private sealed trait Multivalue

  private final case object MultivalueYes extends Multivalue
  private final case object MultivalueNo extends Multivalue

  private final object IsMultivalue {
    def unapply(multivalue: Option[String]): Option[Multivalue] =
      multivalue match {
        case Some(IsFalseish()) | None => Some(MultivalueNo)
        case Some(IsTrueish())         => Some(MultivalueYes)
        case _                         => None
      }
  }

  private final object HasTextExpression {
    def unapply(valueExp: Option[ValueExpr]): Option[Expr] =
      valueExp match {
        case Some(TextExpression(expr)) => Some(expr)
        case None                       => Some(Value)
        case _                          => None
      }
  }

  private final object IsMultiline {
    def unapply(multiline: Option[String]): Boolean =
      multiline match {
        case Some(IsTrueish()) => true
        case _                 => false
      }
  }

  private final object IsNotMultiline {
    def unapply(multiline: Option[String]): Boolean = !IsMultiline.unapply(multiline)
  }

  private sealed trait International

  private final case object InternationalYes extends International
  private final case object InternationalNo extends International

  private final object IsInternational {
    def unapply(international: Option[String]): Option[International] =
      international match {
        case Some(IsFalseish()) | None => Some(InternationalNo)
        case Some(IsTrueish())         => Some(InternationalYes)
        case _                         => None
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

  object IsDerived {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "derived"
  }

  object IsNonSubmissible {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "notsubmitted"
  }

  object IsSummaryInfoOnly {
    def unapply(maybeStandard: String): Boolean = maybeStandard.toLowerCase == "summaryinfoonly"
  }

  object IsTrueish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "true" | "yes" => true
        case _              => false
      }
  }
  object IsFalseish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "false" | "no" => true
        case _              => false
      }
  }

  private final object IsInfoType {
    def unapply(arg: Option[String]): Option[InfoType] =
      arg match {
        case Some(infoTypeString) =>
          infoTypeString.toLowerCase match {
            case "standard"  => Some(StandardInfo)
            case "long"      => Some(LongInfo)
            case "important" => Some(ImportantInfo)
            case "banner"    => Some(BannerInfo)
            case "noformat"  => Some(NoFormat)
            case _           => None
          }
        case None => Some(StandardInfo)
      }
  }

  private final object IsThisAnInfoField {
    def unapply(ignoredArgs: (Option[String], Option[String], Opt[Option[ValueExpr]])): Boolean =
      `type`.contains(InfoRaw)
  }

  private def parse[T: Reads, R](path: String, validate: T => Opt[R]): Opt[Option[R]] = {
    val optMaybeString: Opt[Option[T]] = toOpt((json \ path).validateOpt[T])
    import cats.implicits._
    for {
      maybeString <- optMaybeString.right
      res         <- maybeString.traverse[Opt, R](validate).right

    } yield res
  }
}
