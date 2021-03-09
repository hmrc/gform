/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.traverse._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.{ BasicParsers, FormatParser, OverseasAddressParser, PresentationHintParser, ValueParser }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.RoundingMode._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }

case class MES(
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false)

class FormComponentMaker(json: JsValue) {

  lazy val id: FormComponentId = (json \ "id").as[FormComponentId]
  lazy val `type`: Option[ComponentTypeRaw] = (json \ "type").asOpt[ComponentTypeRaw]
  lazy val label: SmartString = (json \ "label").as[SmartString]

  lazy val optMaybeValueExpr: Opt[Option[ValueExpr]] = parse("value", ValueParser.validate)
  lazy val optMaybeOverseasAddressValue: Opt[Option[OverseasAddress.Value]] =
    parse("value", OverseasAddressParser.validate)

  lazy val optEmailVerification: Opt[EmailVerification] = (json \ "verifiedBy") match {
    case JsDefined(verifiedBy) =>
      EmailVerification.reads.reads(verifiedBy) match {
        case JsSuccess(emailVerification, _) => Right(emailVerification)
        case JsError(error)                  => Left(UnexpectedState(JsError.toJson(error).toString()))
      }
    case JsUndefined() => Right(EmailVerification.noVerification)
  }

  lazy val optMaybeFormatExpr
    : RoundingMode => Option[List[SelectionCriteria]] => EmailVerification => Opt[Option[FormatExpr]] = rm =>
    selectionCriteria =>
      emailVerification => parse("format", FormatParser.validate(rm, selectionCriteria, emailVerification))
  lazy val optMaybePresentationHintExpr: Opt[Option[List[PresentationHint]]] =
    parse("presentationHint", PresentationHintParser.validate)

  lazy val helpText: Option[SmartString] = (json \ "helpText").asOpt[SmartString]
  lazy val optionHints: Option[NonEmptyList[SmartString]] =
    (json \ "hints").asOpt[NonEmptyList[SmartString]]
  lazy val optionHelpText: Option[NonEmptyList[SmartString]] =
    (json \ "optionHelpText").asOpt[NonEmptyList[SmartString]]
  lazy val submitMode: Option[String] = (json \ "submitMode").asOpt[String]
  lazy val choices: Option[List[SmartString]] = (json \ "choices").asOpt[List[SmartString]]

  lazy val revealingFieldsJson: Option[List[List[JsValue]]] =
    (json \ "revealingFields").asOpt[List[List[JsValue]]]
  lazy val revealingFields: Option[List[List[FormComponentMaker]]] =
    revealingFieldsJson.map(_.map(_.map(new FormComponentMaker(_))))

  lazy val idType: Option[String] = (json \ "idType").asOpt[String]
  lazy val idNumber: Opt[Option[ValueExpr]] = parse("idNumber", ValueParser.validate)
  lazy val regimeType: Option[String] = (json \ "regimeType").asOpt[String]

  lazy val fieldsJson: Option[List[JsValue]] = (json \ "fields").asOpt[List[JsValue]]
  lazy val errorMessage: Option[SmartString] = (json \ "errorMessage").asOpt[SmartString]

  lazy val fields: Option[List[FormComponentMaker]] = fieldsJson.map(_.map(new FormComponentMaker(_)))
  lazy val optIncludeIf: Opt[Option[IncludeIf]] = toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
  lazy val optValidIf: Opt[Option[ValidIf]] = toOpt((json \ "validIf").validateOpt[ValidIf], "/validIf")
  lazy val optValidators: Opt[List[FormComponentValidator]] =
    toOpt((json \ "validators").validateOpt[List[FormComponentValidator]], "/validators").map(_.toList.flatten)
  lazy val mandatory: Option[String] = (json \ "mandatory").asOpt[String]
  lazy val mandatoryLine2: Option[String] = (json \ "line2Mandatory").asOpt[String]
  lazy val mandatoryCity: Option[String] = (json \ "cityMandatory").asOpt[String]
  lazy val mandatoryPostcode: Option[String] = (json \ "postcodeMandatory").asOpt[String]
  lazy val multiline: Option[String] = (json \ "multiline").asOpt[String]
  lazy val optRows: Opt[Option[Int]] = parse("rows", BasicParsers.validateNonZeroPositiveNumber)
  lazy val displayCharCount: Option[String] = (json \ "displayCharCount").asOpt[String]
  lazy val displayWidth: Option[String] = (json \ "displayWidth").asOpt[String]
  lazy val toUpperCase: UpperCaseBoolean = (json \ "toUpperCase").asOpt[UpperCaseBoolean].getOrElse(IsNotUpperCase)
  lazy val prefix: Option[SmartString] = (json \ "prefix").asOpt[SmartString]
  lazy val suffix: Option[SmartString] = (json \ "suffix").asOpt[SmartString]
  lazy val roundingMode: RoundingMode = (json \ "round").asOpt[RoundingMode].getOrElse(RoundingMode.defaultRoundingMode)
  lazy val multivalue: Option[String] = (json \ "multivalue").asOpt[String]
  lazy val total: Option[String] = (json \ "total").asOpt[String]
  lazy val international: Option[String] = (json \ "international").asOpt[String]
  lazy val infoText: Option[SmartString] = (json \ "infoText").asOpt[SmartString]
  lazy val infoType: Option[String] = (json \ "infoType").asOpt[String]
  lazy val shortName: Option[SmartString] = (json \ "shortName").asOpt[SmartString]
  lazy val optMaybeRepeatsMax: Opt[Option[Int]] = toOpt((json \ "repeatsMax").validateOpt[Int], "/repeatsMax")
  lazy val optMaybeRepeatsMin: Opt[Option[Int]] = toOpt((json \ "repeatsMin").validateOpt[Int], "/repeatsMin")
  lazy val repeatLabel: Option[SmartString] = (json \ "repeatLabel").asOpt[SmartString]
  lazy val repeatAddAnotherText: Option[SmartString] = (json \ "repeatAddAnotherText").asOpt[SmartString]

  lazy val rangesJson: Option[List[JsValue]] = (json \ "ranges").asOpt[List[JsValue]]
  lazy val ranges: Option[List[Range]] = rangesJson.map(_.map(getTimeRange _))
  lazy val intervalMins: Option[Int] = (json \ "intervalMins").asOpt[Int]
  lazy val optInstruction: Opt[Option[Instruction]] =
    toOpt((json \ "instruction").validateOpt[Instruction], "/instruction")
  lazy val optSelectionCriteria: Opt[Option[List[SelectionCriteria]]] =
    json \ "selectionCriteria" match {
      case JsDefined(JsArray(selectionCriterias)) =>
        val r = selectionCriterias.toList
          .map { selectionCriteria =>
            SelectionCriteria.reads.reads(selectionCriteria) match {
              case JsSuccess(selectionCriteria, _) => Right(selectionCriteria)
              case JsError(error)                  => Left(UnexpectedState(JsError.toJson(error).toString()))
            }
          }
          .partition(_.isLeft) match {
          case (Nil, rights) => Right(for (Right(i) <- rights) yield i)
          case (lefts, _)    => Left(for (Left(s)   <- lefts) yield s)
        }

        r match {
          case Right(r) => Right(Some(r))
          case Left(e)  => Left(UnexpectedState(e.map(_.error).mkString(" ")))
        }

      case JsUndefined() => Right(None)
    }

  def optFieldValue(): Opt[FormComponent] =
    for {
      presHint    <- optMaybePresentationHintExpr
      mes         <- optMES
      ct          <- componentTypeOpt
      validators  <- optValidators
      instruction <- optInstruction
      includeIf   <- optIncludeIf
      validIf     <- optValidIf
    } yield mkFieldValue(presHint, mes, ct, validators, instruction, includeIf, validIf)

  private def toOpt[A](result: JsResult[A], pathPrefix: String): Opt[A] =
    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) =>
        UnexpectedState(errors
          .map {
            case (path, validationErrors) =>
              s"Path: $pathPrefix${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
          }
          .mkString(",")).asLeft
    }

  private def mkFieldValue(
    presHint: Option[List[PresentationHint]],
    mes: MES,
    ct: ComponentType,
    validators: List[FormComponentValidator],
    instruction: Option[Instruction],
    includeIf: Option[IncludeIf],
    validIf: Option[ValidIf]
  ): FormComponent =
    FormComponent(
      id = id,
      `type` = ct,
      label = label,
      helpText = helpText,
      shortName = shortName,
      includeIf = includeIf,
      validIf = validIf,
      mandatory = mes.mandatory,
      editable = mes.editable,
      submissible = mes.submissible,
      derived = mes.derived,
      onlyShowOnSummary = mes.onlyShowOnSummary,
      presentationHint = presHint,
      errorMessage = errorMessage,
      validators = validators,
      instruction = instruction
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
        s"Expected 'standard', 'summaryinfoonly', 'notsubmitted', 'readonly' or 'info' string or nothing for submitMode and expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise").asLeft
    // format: on
  }

  private lazy val componentTypeOpt: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None     => textOpt
    case Some(DateRaw)            => dateOpt
    case Some(AddressRaw)         => addressOpt
    case Some(GroupRaw)           => groupOpt
    case Some(ChoiceRaw)          => choiceOpt
    case Some(RevealingChoiceRaw) => revealingChoiceOpt
    case Some(FileUploadRaw)      => fileUploadOpt
    case Some(InfoRaw)            => infoOpt
    case Some(HmrcTaxPeriodRaw)   => hmrcTaxPeriodOpt
    case Some(TimeRaw)            => timeOpt
    case Some(OverseasAddressRaw) => overseasAddressOpt
  }

  lazy val textOpt: Opt[ComponentType] = {
    for {
      emailVerification <- optEmailVerification
      selectionCriteria <- optSelectionCriteria
      maybeFormatExpr   <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
      maybeValueExpr    <- optMaybeValueExpr
      rows              <- optRows
      result <- createObject(
                 maybeFormatExpr,
                 maybeValueExpr,
                 multiline,
                 displayWidth,
                 toUpperCase,
                 prefix,
                 suffix,
                 rows.getOrElse(TextArea.defaultRows),
                 IsDisplayCharCountTrue.unapply(displayCharCount),
                 json
               )
    } yield result
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
        emailVerification <- optEmailVerification
        selectionCriteria <- optSelectionCriteria
        maybeFormatExpr   <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
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

    val fieldValuesOpt: Opt[List[FormComponent]] = fields.traverse(_.optFieldValue())

    for {
      emailVerification <- optEmailVerification
      selectionCriteria <- optSelectionCriteria
      fieldValues       <- fieldValuesOpt.right
      format            <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification).right
      repMax            <- optMaybeRepeatsMax
      repMin            <- optMaybeRepeatsMin
      group             <- validateRepeatsAndBuildGroup(repMax, repMin, fieldValues)
    } yield group
  }

  private def validateRepeatsAndBuildGroup(
    repMax: Option[Int],
    repMin: Option[Int],
    fields: List[FormComponent]): Either[UnexpectedState, Group] =
    (repMax, repMin) match {
      case (Some(repMaxV), Some(repMinV)) if repMaxV < repMinV =>
        UnexpectedState(s"""repeatsMax should be higher than repeatsMin in Group field""").asLeft
      case (Some(repMaxV), Some(repMinV)) if repMinV < 0 =>
        UnexpectedState(s"""repeatsMin in Group field cannot be a negative number""").asLeft
      case (_, repeatsMin) =>
        val fieldsMandatory =
          if (repeatsMin.getOrElse(None) == 0) fields.map(field => field.copy(mandatory = false)) else fields
        Group(fieldsMandatory, repMax, repMin, repeatLabel, repeatAddAnotherText).asRight
    }

  private def toSmartString(stringEn: String, stringCy: String) =
    SmartString(LocalisedString(Map(LangADT.En -> stringEn, LangADT.Cy -> stringCy)), Nil)

  private lazy val choiceOpt: Opt[Choice] = {
    for {
      emailVerification <- optEmailVerification
      selectionCriteria <- optSelectionCriteria
      maybeFormatExpr   <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
      maybeValueExpr    <- optMaybeValueExpr
      oChoice: Opt[Choice] = (maybeFormatExpr, choices, multivalue, maybeValueExpr) match {
        // format: off
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) => Choice(Checkbox, NonEmptyList(x, xs),          Vertical,   selections, optionHints, optionHelpText).asRight
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(Radio,    NonEmptyList(x, xs),          Vertical,   selections, optionHints, optionHelpText).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) => Choice(Checkbox, NonEmptyList(x, xs),          Horizontal, selections, optionHints, optionHelpText).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(Radio,    NonEmptyList(x, xs),          Horizontal, selections, optionHints, optionHelpText).asRight
        case (IsOrientation(YesNoOrientation),      None,          IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(YesNo,    NonEmptyList.of(toSmartString("Yes","Iawn"), toSmartString("No","Na")), Horizontal, selections, optionHints, optionHelpText).asRight
        case (IsOrientation(YesNoOrientation),      _,             _,                           Selections(selections)) => Choice(YesNo,    NonEmptyList.of(toSmartString("Yes","Iawn"), toSmartString("No","Na")), Horizontal, selections, optionHints, optionHelpText).asRight
        // format: on
        case (invalidFormat, invalidChoices, invalidMultivalue, invalidValue) =>
          UnexpectedState(s"""|Unsupported combination of 'format, choices, multivalue and value':
                              |Format     : $invalidFormat
                              |Choices    : $invalidChoices
                              |Multivalue : $invalidMultivalue
                              |Value      : $invalidValue
                              |optionHints: $optionHints
                              |optionHelpText: $optionHelpText
                              |""".stripMargin).asLeft
      }
      result <- oChoice.right
    } yield result
  }

  private lazy val revealingChoiceOpt: Opt[RevealingChoice] = for {
    maybeValueExpr <- optMaybeValueExpr
    result         <- createRevealingChoice(maybeValueExpr)
  } yield result

  private def createRevealingChoice(maybeValueExpr: Option[ValueExpr]): Opt[RevealingChoice] = {

    val maybeRevealingFields: Option[Opt[List[List[FormComponent]]]] =
      revealingFields.map(_.traverse(_.traverse(_.optFieldValue())))

    (choices, maybeValueExpr, maybeRevealingFields) match {
      case (Some(options), Selections(selections), Some(Right(revealingFields))) =>
        def mkError[A](error: String): Either[String, A] = s"RevealingChoice error: $error".asLeft

        val selectionE: Either[String, Option[Int]] = selections match {
          case Nil              => Right(None)
          case selection :: Nil => Right(Some(selection))
          case _ :: _ =>
            val sel = selections.mkString(",")
            mkError(s"Only single choice can be selected, but $sel has been defined")
        }

        def rcFromSelection(selections: List[Boolean]): Either[String, RevealingChoice] = {
          val hints =
            optionHints.fold[List[Option[SmartString]]](List.fill(revealingFields.size)(None))(_.toList.map(hint =>
              if (hint.nonEmpty) Some(hint) else None))

          val revealingChoiceElements: List[RevealingChoiceElement] =
            options
              .zip(revealingFields)
              .zip(selections)
              .zip(hints)
              .map {
                case (((choice, fields), selected), hint) =>
                  RevealingChoiceElement(choice, fields, hint, selected)
              }

          (revealingChoiceElements, multivalue) match {
            case (x :: xs, IsMultivalue(MultivalueYes)) => RevealingChoice(NonEmptyList(x, xs), true).asRight
            case (x :: xs, IsMultivalue(MultivalueNo))  => RevealingChoice(NonEmptyList(x, xs), false).asRight
            case (_ :: _, _) =>
              mkError(s"'$multivalue' is not a valid value for the revealing choice multivalue property")
            case _ => mkError("At least one choice needs to be specified")
          }
        }

        def construcRevealingChoice(maybeSelection: Option[Int]): Either[String, RevealingChoice] =
          if (optionHints.fold(false)(_.size =!= options.size)) {
            mkError(
              s"Number of 'choices': ${options.size} and number of 'hints': ${optionHints.map(_.size)} does not match. They need to be identical.")
          } else if (options.size =!= revealingFields.size) {
            mkError(
              s"Number of 'choices': ${options.size} and number of 'revealingFields': ${revealingFields.size} does not match. They need to be identical.")
          } else {

            val initialSelections = List.fill(options.size)(false)
            val selectionsE = maybeSelection.fold[Either[String, List[Boolean]]](initialSelections.asRight) {
              selection =>
                if (initialSelections.isDefinedAt(selection)) {
                  initialSelections.updated(selection, true).asRight
                } else mkError(s"Selection index $selection doesn't correspond to any of the choices")
            }

            for {
              selections <- selectionsE
              rc         <- rcFromSelection(selections)
            } yield rc

          }

        val res = for {
          selection <- selectionE
          rc        <- construcRevealingChoice(selection)
        } yield rc

        res.leftMap(UnexpectedState)
      case _ =>
        UnexpectedState(s"""|Wrong revealing choice definition
                            |choices : ${choices.getOrElse("MISSING - This field must be provided")}
                            |selections: ${maybeValueExpr.getOrElse("")}
                            |revealingFields: ${revealingFields
                             .getOrElse("MISSING - This field must be provided")}""".stripMargin).asLeft
    }
  }

  private lazy val hmrcTaxPeriodOpt: Opt[HmrcTaxPeriod] = (idType, idNumber, regimeType) match {
    case (Some(a), Right(Some(b: TextExpression)), Some(c)) => HmrcTaxPeriod(IdType(a), b.expr, RegimeType(c)).asRight
    case _                                                  => UnexpectedState(s"""
                                 |Wrong HmrcTaxPeriod definition:
                                 |IdType     : $idType
                                 |IdNumber   : $idNumber
                                 |RegimeType : $regimeType
                                 |""".stripMargin).asLeft
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

  private lazy val timeOpt: Opt[Time] = (ranges, intervalMins) match {
    case (Some(ranges), Some(mins)) if !ranges.exists(r => r.startTime.time.isAfter(r.endTime.time)) && mins >= 1 =>
      Time(ranges, IntervalMins(mins)).asRight
    case _ =>
      UnexpectedState(s"""
                         |Wrong Time definition:
                         |Ranges must be in valid HH:mm format with startTime same as or before endTime  : $ranges
                         |IntervalMins must be greater than 0 minutes  : $intervalMins
                         |""".stripMargin).asLeft
  }

  private val overseasAddressOpt: Opt[OverseasAddress] = {

    import OverseasAddress.Configurable._
    for {
      maybeOverseasAddressValue <- optMaybeOverseasAddressValue
      line2                     <- OverseasAddressParser.mandatoryField(mandatoryLine2, Mandatory.Line2)
      city                      <- OverseasAddressParser.optionalField(mandatoryCity, Optional.City)
      postcode                  <- OverseasAddressParser.mandatoryField(mandatoryPostcode, Mandatory.Postcode)
    } yield {
      val mandatoryFields: List[Mandatory] = List(line2, postcode).flatten
      val optionalFields: List[Optional] = List(city).flatten

      OverseasAddress(mandatoryFields, optionalFields, maybeOverseasAddressValue)
    }
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

  private final object IsOrientation {
    def unapply(orientation: Option[FormatExpr]): Option[ChoiceOrientation] =
      orientation match {
        case Some(OrientationFormat("vertical")) | None => Some(VerticalOrientation)
        case Some(OrientationFormat("horizontal"))      => Some(HorizontalOrientation)
        case Some(OrientationFormat("yesno"))           => Some(YesNoOrientation)
        case _                                          => None
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
    val optMaybeString: Opt[Option[T]] = toOpt((json \ path).validateOpt[T], path)
    import cats.implicits._
    for {
      maybeString <- optMaybeString.right
      res         <- maybeString.traverse[Opt, R](validate).right

    } yield res
  }

  private def getTimeRange(json: JsValue): Range = {
    val formatter = DateTimeFormatter.ofPattern("HH:mm")
    val startTime = (json \ "startTime").as[String]
    val endTime = (json \ "endTime").as[String]
    Range(
      StartTime(Range.stringToLocalTime(formatter, startTime)),
      EndTime(Range.stringToLocalTime(formatter, endTime)))
  }

}
