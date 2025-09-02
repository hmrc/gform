/*
 * Copyright 2023 HM Revenue & Customs
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
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.{ AddressParser, BasicParsers, FormatParser, LabelSizeParser, OverseasAddressParser, PresentationHintParser, PriorityTypeParser, SummaryListParser, ValueParser }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import SmartString._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.KeyDisplayWidth.KeyDisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayInSummary

case class MES(
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false
)

class FormComponentMaker(json: JsValue) {

  lazy val id: FormComponentId = (json \ "id").as[FormComponentId]
  lazy val `type`: Option[ComponentTypeRaw] = (json \ "type").asOpt[ComponentTypeRaw]
  lazy val optLabel: Opt[Option[SmartString]] = toOpt((json \ "label").validateOpt[SmartString], "/label")

  lazy val optMaybeValueExpr: Opt[Option[ValueExpr]] = parse("value", ValueParser.validate)

  lazy val optEmailVerification: Opt[EmailVerification] = (json \ "verifiedBy") match {
    case JsDefined(verifiedBy) =>
      EmailVerification.reads.reads(verifiedBy) match {
        case JsSuccess(emailVerification, _) => Right(emailVerification)
        case JsError(error)                  => Left(UnexpectedState(JsError.toJson(error).toString()))
      }
    case _: JsUndefined => Right(EmailVerification.noVerification)
  }

  lazy val optNotPII: Opt[Boolean] = json \ "notPII" match {
    case JsDefined(JsTrue) => Right(true)
    case _                 => Right(false)
  }

  lazy val optExtraLetterSpacing: Opt[Option[Boolean]] = (json \ "extraLetterSpacing") match {
    case JsDefined(JsTrue)  => Right(Some(true))
    case JsDefined(JsFalse) => Right(Some(false))
    case _                  => Right(None)
  }

  lazy val optMaybeFormatExpr
    : RoundingMode => Option[List[SelectionCriteria]] => EmailVerification => Opt[Option[FormatExpr]] = rm =>
    selectionCriteria =>
      emailVerification => parse("format", FormatParser.validate(rm, selectionCriteria, emailVerification))
  lazy val optMaybePresentationHintExpr: Opt[Option[List[PresentationHint]]] =
    parse("presentationHint", PresentationHintParser.validate)

  lazy val optHelpText: Opt[Option[SmartString]] = toOpt((json \ "helpText").validateOpt[SmartString], "/helpText")
  lazy val optionHints: Option[NonEmptyList[SmartString]] =
    (json \ "hints").asOpt[NonEmptyList[SmartString]]
  lazy val optionHelpText: Option[NonEmptyList[SmartString]] =
    (json \ "optionHelpText").asOpt[NonEmptyList[SmartString]]
  lazy val submitMode: Option[String] = (json \ "submitMode").asOpt[String]
  lazy val choicesOpt: Opt[Option[List[OptionData]]] =
    toOpt((json \ "choices").validateOpt[List[OptionData]], "/choices")

  lazy val revealingFieldsJson: Option[List[List[JsValue]]] =
    (json \ "revealingFields").asOpt[List[List[JsValue]]]
  lazy val revealingFields: Option[List[List[FormComponentMaker]]] =
    revealingFieldsJson.map(_.map(_.map(new FormComponentMaker(_))))

  lazy val idType: Option[String] = (json \ "idType").asOpt[String]
  lazy val idNumber: Opt[Option[ValueExpr]] = parse("idNumber", ValueParser.validate)
  lazy val regimeType: Option[String] = (json \ "regimeType").asOpt[String]

  lazy val fieldsJson: Option[List[JsValue]] = (json \ "fields").asOpt[List[JsValue]]
  lazy val optErrorMessage: Opt[Option[SmartString]] =
    toOpt((json \ "errorMessage").validateOpt[SmartString], "/errorMessage")

  lazy val fields: Option[List[FormComponentMaker]] = fieldsJson.map(_.map(new FormComponentMaker(_)))
  lazy val optIncludeIf: Opt[Option[IncludeIf]] = toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
  lazy val optValidIf: Opt[Option[ValidIf]] = toOpt((json \ "validIf").validateOpt[ValidIf], "/validIf")
  lazy val optValidators: Opt[List[FormComponentValidator]] =
    toOpt((json \ "validators").validateOpt[List[FormComponentValidator]], "/validators").map(_.toList.flatten)
  lazy val optMandatory: Opt[Option[Boolean]] = toOpt((json \ "mandatory").validateOpt[Boolean], "/mandatory")
  lazy val optMandatoryLine2: Opt[Option[Boolean]] =
    toOpt((json \ "line2Mandatory").validateOpt[Boolean], "/line2Mandatory")
  lazy val optMandatoryCity: Opt[Option[Boolean]] =
    toOpt((json \ "cityMandatory").validateOpt[Boolean], "/cityMandatory")
  lazy val optMandatoryPostcode: Opt[Option[Boolean]] =
    toOpt((json \ "postcodeMandatory").validateOpt[Boolean], "/postcodeMandatory")
  lazy val optMultiline: Opt[Option[Boolean]] = toOpt((json \ "multiline").validateOpt[Boolean], "/multiline")
  lazy val dataThreshold: Option[Int] = (json \ "dataThreshold").asOpt[Int]
  lazy val optRows: Opt[Option[Int]] = parse("rows", BasicParsers.validateNonZeroPositiveNumber)
  lazy val optDisplayCharCount: Opt[Option[Boolean]] =
    toOpt((json \ "displayCharCount").validateOpt[Boolean], "/displayCharCount")
  lazy val displayWidth: Option[String] = (json \ "displayWidth").asOpt[String]
  lazy val toUpperCase: UpperCaseBoolean = (json \ "toUpperCase").asOpt[UpperCaseBoolean].getOrElse(IsNotUpperCase)
  lazy val displayInSummary: DisplayInSummary =
    (json \ "displayInSummary").asOpt[DisplayInSummary].getOrElse(DisplayInSummary(IsFalse))
  lazy val prefix: Option[SmartString] = (json \ "prefix").asOpt[SmartString]
  lazy val suffix: Option[SmartString] = (json \ "suffix").asOpt[SmartString]
  lazy val optPriority: Opt[Option[Priority]] = parse("priority", PriorityTypeParser.validate)
  lazy val roundingMode: RoundingMode = (json \ "round").asOpt[RoundingMode].getOrElse(RoundingMode.defaultRoundingMode)
  lazy val optMultivalue: Opt[Option[Boolean]] = toOpt((json \ "multivalue").validateOpt[Boolean], "/multivalue")
  lazy val optDisplayInSummary: Opt[Option[DisplayInSummary]] =
    toOpt((json \ "displayInSummary").validateOpt[DisplayInSummary], "/displayInSummary")
  lazy val optHideChoicesSelected: Opt[Option[Boolean]] =
    toOpt((json \ "hideChoicesSelected").validateOpt[Boolean], "/hideChoicesSelected")
  lazy val total: Option[String] = (json \ "total").asOpt[String]
  lazy val optInternational: Opt[Option[Boolean]] =
    toOpt((json \ "international").validateOpt[Boolean], "/international")
  lazy val optInfoText: Opt[SmartString] = toOpt((json \ "infoText").validate[SmartString], "/infoText")
  lazy val infoType: Option[String] = (json \ "infoType").asOpt[String]
  lazy val optShortName: Opt[Option[SmartString]] = toOpt((json \ "shortName").validateOpt[SmartString], "/shortName")
  lazy val optErrorShortName: Opt[Option[SmartString]] =
    toOpt((json \ "errorShortName").validateOpt[SmartString], "/errorShortName")
  lazy val optErrorShortNameStart: Opt[Option[SmartString]] =
    toOpt((json \ "errorShortNameStart").validateOpt[SmartString], "/errorShortNameStart")
  lazy val optErrorExample: Opt[Option[SmartString]] =
    toOpt((json \ "errorExample").validateOpt[SmartString], "/errorExample")
  lazy val optMaybeRepeatsMax: Opt[Option[Int]] = toOpt((json \ "repeatsMax").validateOpt[Int], "/repeatsMax")
  lazy val optMaybeRepeatsMin: Opt[Option[Int]] = toOpt((json \ "repeatsMin").validateOpt[Int], "/repeatsMin")
  lazy val repeatLabel: Option[SmartString] = (json \ "repeatLabel").asOpt[SmartString]
  lazy val repeatAddAnotherText: Option[SmartString] = (json \ "repeatAddAnotherText").asOpt[SmartString]

  lazy val rangesJson: Option[List[JsValue]] = (json \ "ranges").asOpt[List[JsValue]]
  lazy val ranges: Option[List[Range]] = rangesJson.map(_.map(getTimeRange _))
  lazy val intervalMins: Option[Int] = (json \ "intervalMins").asOpt[Int]
  lazy val optInstruction: Opt[Option[Instruction]] =
    toOpt((json \ "instruction").validateOpt[Instruction], "/instruction")
  lazy val optChooseAddressLabel: Opt[Option[SmartString]] =
    toOpt((json \ "chooseAddressLabel").validateOpt[SmartString], "/chooseAddressLabel")
  lazy val optConfirmAddressLabel: Opt[Option[SmartString]] =
    toOpt((json \ "confirmAddressLabel").validateOpt[SmartString], "/confirmAddressLabel")
  lazy val optEnterAddressLabel: Opt[Option[SmartString]] =
    toOpt((json \ "enterAddressLabel").validateOpt[SmartString], "/enterAddressLabel")

  lazy val reference: Opt[Option[ValueExpr]] = parse("reference", ValueParser.validate)
  lazy val amountInPence: Opt[Option[ValueExpr]] = parse("amountInPence", ValueParser.validate)
  lazy val isStartButton: Opt[Option[Boolean]] =
    toOpt((json \ "isStartButton").validateOpt[Boolean], "/isStartButton")
  lazy val classes: Opt[Option[String]] =
    toOpt((json \ "classes").validateOpt[String], "/classes")

  lazy val optPageIdsToDisplayOnChange: Opt[Option[List[PageId]]] =
    json \ "pageIdsToDisplayOnChange" match {
      case JsDefined(JsArray(pageIds)) =>
        val r = pageIds.toList
          .map { pageId =>
            PageId.format.reads(pageId) match {
              case JsSuccess(page, _) => Right(page)
              case JsError(error)     => Left(UnexpectedState(JsError.toJson(error).toString()))
            }
          }
          .partition(_.isLeft) match {
          case (Nil, rights) => Right(for (Right(i) <- rights) yield i)
          case (lefts, _)    => Left(for (Left(s) <- lefts) yield s)
        }
        r match {
          case Right(r) => Right(Some(r))
          case Left(e)  => Left(UnexpectedState(e.map(_.error).mkString(" ")))
        }

      case JsDefined(notAnArray) =>
        Left(UnexpectedState(s"'pageIdsToDisplayOnChange' needs to be an array, got: $notAnArray"))
      case _: JsUndefined => Right(None)
    }

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
          case (lefts, _)    => Left(for (Left(s) <- lefts) yield s)
        }

        r match {
          case Right(r) => Right(Some(r))
          case Left(e)  => Left(UnexpectedState(e.map(_.error).mkString(" ")))
        }

      case JsDefined(notAnArray) => Left(UnexpectedState(s"'selectionCriteria' needs to be an array, got: $notAnArray"))

      case _: JsUndefined => Right(None)
    }

  lazy val dividerPositon: Option[DividerPosition] =
    (json \ "dividerPosition").toOption.flatMap {
      case JsNumber(number) => Some(DividerPosition.Number(number.toIntExact))
      case JsString(value)  => Some(DividerPosition.Value(value))
      case _                => Option.empty[DividerPosition]
    }

  lazy val dividerText: LocalisedString = (json \ "dividerText")
    .asOpt[LocalisedString]
    .getOrElse(LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")))
  lazy val noneChoice: Option[NoneChoice] = (json \ "noneChoice").asOpt[NoneChoice]
  lazy val noneChoiceError: Option[LocalisedString] = (json \ "noneChoiceError").asOpt[LocalisedString]

  lazy val optLabelSize: Opt[Option[LabelSize]] =
    parse("labelSize", LabelSizeParser.validate)

  lazy val optCountryLookup: Opt[Option[Boolean]] =
    toOpt((json \ "countryLookup").validateOpt[Boolean], "/countryLookup")
  lazy val optCountryDisplayed: Opt[Option[Boolean]] =
    toOpt((json \ "countryDisplayed").validateOpt[Boolean], "/countryDisplayed")
  lazy val optCountyDisplayed: Opt[Option[Boolean]] =
    toOpt((json \ "countyDisplayed").validateOpt[Boolean], "/countyDisplayed")

  private def getValueRow(json: JsValue): Opt[MiniSummaryRow] =
    for {
      key       <- toOpt((json \ "key").validateOpt[SmartString], "/key")
      value     <- toOpt((json \ "value").validate[String], "/value").flatMap(SummaryListParser.validate)
      includeIf <- toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
      pageId    <- toOpt((json \ "pageId").validateOpt[PageId], "/pageId")
      taskId    <- toOpt((json \ "taskId").validateOpt[TaskId], "/taskId")
    } yield MiniSummaryRow.ValueRow(key, value, includeIf, pageId, taskId)

  private def getSmartStringRow(json: JsValue): Opt[MiniSummaryRow] =
    for {
      key       <- toOpt((json \ "key").validateOpt[SmartString], "/key")
      value     <- toOpt((json \ "value").validate[SmartString], "/value")
      includeIf <- toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
      pageId    <- toOpt((json \ "pageId").validateOpt[PageId], "/pageId")
      taskId    <- toOpt((json \ "taskId").validateOpt[TaskId], "/taskId")
    } yield MiniSummaryRow.SmartStringRow(key, value, includeIf, pageId, taskId)

  private def getHeaderRow(json: JsValue): Opt[MiniSummaryRow] =
    for {
      header <- toOpt((json \ "header").validate[SmartString], "/header")
    } yield MiniSummaryRow.HeaderRow(header)

  private def getATLRow(json: JsValue): Opt[MiniSummaryRow] =
    for {
      atlId     <- toOpt((json \ "atlId").validate[FormComponentId], "/atlId")
      includeIf <- toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
      atlRows   <- rows(json, "repeat")
    } yield MiniSummaryRow.ATLRow(atlId, includeIf, atlRows)

  def rows(json: JsValue, field: String): Opt[List[MiniSummaryRow]] = {
    def loop(a: Opt[MiniSummaryRow], b: => Opt[MiniSummaryRow]): Opt[MiniSummaryRow] =
      a match {
        case Right(r) => Right(r)
        case _        => b
      }
    for {
      jsValues <- toOpt((json \ field).validate[List[JsValue]], s"/$field")
      rs <- jsValues
              .traverse(j => loop(loop(loop(getValueRow(j), getHeaderRow(j)), getATLRow(j)), getSmartStringRow(j)))
    } yield rs
  }

  lazy val summaryListOpt: Opt[MiniSummaryList] = {
    for {
      keyDisplayWidth <- toOpt((json \ "keyDisplayWidth").validateOpt[KeyDisplayWidth], "/keyDisplayWidth")
      ms              <- rows(json, "rows").map(rs => MiniSummaryList(rs, displayInSummary, keyDisplayWidth))
    } yield ms
  }

  lazy val tableCompOpt: Opt[TableComp] = {
    def getValueRow(json: JsValue): Opt[TableValue] =
      for {
        cssClass <- toOpt((json \ "class").validateOpt[String], "/class")
        colspan  <- toOpt((json \ "colspan").validateOpt[Int], "/colspan")
        rowspan  <- toOpt((json \ "rowspan").validateOpt[Int], "/rowspan")
        value    <- toOpt((json \ "value").validate[SmartString], "/value")
      } yield TableValue(value, cssClass, colspan, rowspan)
    def values(json: JsValue): Opt[List[TableValue]] =
      toOpt((json \ "values").validate[List[JsValue]], "/values").flatMap(_.traverse(getValueRow))
    def getRow(json: JsValue): Opt[TableValueRow] =
      for {
        includeIf <- toOpt((json \ "includeIf").validateOpt[IncludeIf], "/includeIf")
        dynamic   <- toOpt((json \ "dynamic").validateOpt[Dynamic], "/dynamic")
        values    <- values(json)
      } yield TableValueRow(values, includeIf, dynamic)

    def rows(json: JsValue): Opt[List[TableValueRow]] =
      for {
        jsValues <- toOpt((json \ "rows").validate[List[JsValue]], s"/rows")
        rs       <- jsValues.traverse(getRow)
      } yield rs

    for {
      rows           <- rows(json)
      header         <- toOpt((json \ "header").validate[List[TableHeaderCell]], "/header")
      summaryValue   <- toOpt((json \ "summaryValue").validate[SmartString], "/summaryValue")
      caption        <- toOpt((json \ "caption").validateOpt[String], "/caption")
      captionClasses <- toOpt((json \ "captionClasses").validateOpt[String].map(_.getOrElse("")), "/captionClasses")
      smallText      <- toOpt((json \ "smallText").validateOpt[Boolean].map(_.getOrElse(false)), "/smallText")
      classes        <- toOpt((json \ "classes").validateOpt[String].map(_.getOrElse("")), "/classes")
      firstCellIsHeader <-
        toOpt(
          (json \ "firstCellIsHeader").validateOpt[String].map(_.getOrElse("false")).map(_.toBoolean),
          "/firstCellIsHeader"
        )
    } yield {
      val enhancedClasses: String = if (smallText) s"$classes govuk-table--small-text-until-tablet".trim else classes
      TableComp(header, rows, summaryValue, caption, captionClasses, enhancedClasses, firstCellIsHeader)
    }
  }

  def optFieldValue(): Opt[FormComponent] =
    for {
      label                    <- optLabel
      helpText                 <- optHelpText
      shortName                <- optShortName
      errorMessage             <- optErrorMessage
      errorShortName           <- optErrorShortName
      errorShortNameStart      <- optErrorShortNameStart
      errorExample             <- optErrorExample
      presHint                 <- optMaybePresentationHintExpr
      mes                      <- optMES
      ct                       <- componentTypeOpt
      validators               <- optValidators
      instruction              <- optInstruction
      includeIf                <- optIncludeIf
      validIf                  <- optValidIf
      labelSize                <- optLabelSize
      notPII                   <- optNotPII
      extraLetterSpacing       <- optExtraLetterSpacing
      displayInSummary         <- optDisplayInSummary
      pageIdsToDisplayOnChange <- optPageIdsToDisplayOnChange
    } yield mkFieldValue(
      label,
      helpText,
      shortName,
      errorMessage,
      errorShortName,
      errorShortNameStart,
      errorExample,
      presHint,
      mes,
      ct,
      validators,
      instruction,
      includeIf,
      validIf,
      labelSize,
      notPII,
      extraLetterSpacing,
      displayInSummary,
      pageIdsToDisplayOnChange
    )

  private def toOpt[A](result: JsResult[A], pathPrefix: String): Opt[A] =
    result match {
      case JsSuccess(a, _) => a.asRight
      case JsError(errors) =>
        UnexpectedState(
          errors
            .map { case (path, validationErrors) =>
              s"Path: $pathPrefix${path.toString}, Errors: ${validationErrors.map(_.messages.mkString(",")).mkString(",")}"
            }
            .mkString(",")
        ).asLeft
    }

  private def mkFieldValue(
    label: Option[SmartString],
    helpText: Option[SmartString],
    shortName: Option[SmartString],
    errorMessage: Option[SmartString],
    errorShortName: Option[SmartString],
    errorShortNameStart: Option[SmartString],
    errorExample: Option[SmartString],
    presHint: Option[List[PresentationHint]],
    mes: MES,
    ct: ComponentType,
    validators: List[FormComponentValidator],
    instruction: Option[Instruction],
    includeIf: Option[IncludeIf],
    validIf: Option[ValidIf],
    labelSize: Option[LabelSize],
    notPII: Boolean,
    extraLetterSpacing: Option[Boolean],
    displayInSummary: Option[DisplayInSummary],
    pageIdsToDisplayOnChange: Option[List[PageId]]
  ): FormComponent =
    FormComponent(
      id = id,
      `type` = ct,
      label = label.getOrElse(
        // This value is replaced later in PageHeadingHelper by "Page title" (if all conditions are met)
        SmartString(
          LocalisedString.empty,
          List.empty[Expr]
        )
      ),
      isPageHeading = label.isEmpty && ct.cast[InformationMessage].isEmpty && ct.cast[PostcodeLookup].isEmpty,
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
      instruction = instruction,
      labelSize = labelSize,
      errorShortName = errorShortName,
      errorShortNameStart = errorShortNameStart,
      errorExample = errorExample,
      notPII = notPII,
      extraLetterSpacing = extraLetterSpacing,
      displayInSummary = displayInSummary,
      pageIdsToDisplayOnChange = pageIdsToDisplayOnChange
    )

  private lazy val optMES: Opt[MES] = (submitMode, optMandatory, optMaybeValueExpr) match {
    // format: off
    case IsThisAnInfoField()                                                            => MES(mandatory = true,  editable = false, submissible = false, derived = false).asRight
    case (Some(IsStandard()) | None, Right(Some(true))  | Right(None),  _)              => MES(mandatory = true,  editable = true,  submissible = true,  derived = false).asRight
    case (Some(IsReadOnly()),        Right(Some(true))  | Right(None),  _)              => MES(mandatory = true,  editable = false, submissible = true,  derived = false).asRight
    case (Some(IsInfo()),            Right(Some(true))  | Right(None),  _)              => MES(mandatory = true,  editable = false, submissible = false, derived = false).asRight
    case (Some(IsStandard()) | None, Right(Some(false)),         _)                     => MES(mandatory = false, editable = true,  submissible = true,  derived = false).asRight
    case (Some(IsInfo()),            Right(Some(false)),         _)                     => MES(mandatory = false, editable = false, submissible = false, derived = false).asRight
    case (Some(IsDerived()),         Right(Some(true))  | Right(None),  Right(Some(_))) => MES(mandatory = true,  editable = false, submissible = true,  derived = true).asRight
    case (Some(IsDerived()),         Right(Some(false)),         Right(Some(_)))        => MES(mandatory = false, editable = false, submissible = true,  derived = true).asRight
    case (Some(IsNonSubmissible()),  Right(Some(false)) | Right(None),  _)              => MES(mandatory = false, editable = true,  submissible = false, derived = false).asRight
    case (Some(IsNonSubmissible()),  Right(Some(true))  | Right(None),  _)              => MES(mandatory = true,  editable = true,  submissible = false, derived = false).asRight
    case (Some(IsSummaryInfoOnly()), Right(Some(true))  | Right(Some(false)) | Right(None), Right(Some(_))) =>
      MES(mandatory = true,  editable = false, submissible = false, derived = false, onlyShowOnSummary = true).asRight
    case otherwise =>
      UnexpectedState(
        s"Expected 'standard', 'summaryinfoonly', 'notsubmitted', 'readonly' or 'info' string or nothing for submitMode and expected 'true' or 'false' string or nothing for mandatory field value, got: $otherwise").asLeft
    // format: on
  }

  private lazy val componentTypeOpt: Opt[ComponentType] = `type` match {
    case Some(TextRaw) | None     => textOpt
    case Some(CalendarDateRaw)    => calendarDateOpt
    case Some(TaxPeriodDateRaw)   => taxPeriodDateOpt
    case Some(DateRaw)            => dateOpt
    case Some(AddressRaw)         => addressOpt
    case Some(GroupRaw)           => groupOpt
    case Some(ChoiceRaw)          => choiceOpt
    case Some(RevealingChoiceRaw) => revealingChoiceOpt
    case Some(FileUploadRaw)      => fileUploadOpt
    case Some(MultiFileUploadRaw) => multiFileUploadOpt
    case Some(InfoRaw)            => infoOpt
    case Some(HmrcTaxPeriodRaw)   => hmrcTaxPeriodOpt
    case Some(TimeRaw)            => timeOpt
    case Some(OverseasAddressRaw) => overseasAddressOpt
    case Some(PostcodeLookupRaw)  => postcodeLookupOpt
    case Some(SummaryListRaw)     => summaryListOpt
    case Some(TableCompRaw)       => tableCompOpt
    case Some(ButtonRaw)          => buttonOpt
  }

  lazy val textOpt: Opt[ComponentType] = {
    for {
      emailVerification <- optEmailVerification
      selectionCriteria <- optSelectionCriteria
      maybeFormatExpr   <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
      maybeValueExpr    <- optMaybeValueExpr
      rows              <- optRows
      multiline         <- optMultiline
      displayCharCount  <- optDisplayCharCount
      priority          <- optPriority
      result <- createObject(
                  maybeFormatExpr,
                  maybeValueExpr,
                  multiline.getOrElse(false),
                  dataThreshold,
                  displayWidth,
                  toUpperCase,
                  prefix,
                  suffix,
                  priority,
                  rows.getOrElse(TextArea.defaultRows),
                  displayCharCount.getOrElse(true),
                  json
                )
    } yield result
  }

  private val internationalOpt: Opt[Boolean] = optInternational match {
    case Right(IsInternational(InternationalYes)) => true.asRight
    case Right(IsInternational(InternationalNo))  => false.asRight
    case invalidInternational =>
      UnexpectedState(s"""|Unsupported type of value for address field
                          |Id: $id
                          |Total: $invalidInternational""".stripMargin).asLeft
  }

  private def mandatoryCityForNonInternationnal(
    maybeCityMandatory: Option[Address.Configurable.Mandatory],
    international: Boolean
  ): Opt[Unit] =
    if (international && maybeCityMandatory.contains(Address.Configurable.Mandatory.City))
      Left(UnexpectedState("'cityMandatory' is not compatible with 'international' address toggle"))
    else
      Right(())

  private lazy val addressOpt: Opt[Address] = {

    val addressValueOpt = for {
      maybeValue <- toOpt((json \ "value").validateOpt[TextExpression], "/value")
    } yield maybeValue.map(_.expr)

    import Address.Configurable._
    for {
      mandatoryCity   <- optMandatoryCity
      city            <- AddressParser.mandatoryField(mandatoryCity, Mandatory.City)
      international   <- internationalOpt
      _               <- mandatoryCityForNonInternationnal(city, international)
      value           <- addressValueOpt
      countyDisplayed <- optCountyDisplayed
    } yield {
      val mandatoryFields: List[Mandatory] = List(city).flatten
      Address(international, mandatoryFields, countyDisplayed.getOrElse(false), value)
    }
  }

  private lazy val calendarDateOpt: Opt[CalendarDate.type] = Right(CalendarDate)
  private lazy val taxPeriodDateOpt: Opt[TaxPeriodDate.type] = Right(TaxPeriodDate)

  private lazy val postcodeLookupOpt: Opt[PostcodeLookup] = for {
    chooseAddressLabel  <- optChooseAddressLabel
    confirmAddressLabel <- optConfirmAddressLabel
    enterAddressLabel   <- optEnterAddressLabel
  } yield PostcodeLookup(chooseAddressLabel, confirmAddressLabel, enterAddressLabel)

  private lazy val buttonOpt: Opt[Button] = (reference, amountInPence, isStartButton, classes) match {
    case (Right(Some(a: TextExpression)), Right(Some(b: TextExpression)), Right(isStartButton), Right(classes)) =>
      Button(a.expr, b.expr, isStartButton.getOrElse(false), classes).asRight
    case _ => UnexpectedState(s"""
                                 |Wrong button definition:
                                 |reference       : $reference
                                 |amountInPence   : $amountInPence
                                 |""".stripMargin).asLeft
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
      fieldValues       <- fieldValuesOpt
      format            <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
      repMax            <- optMaybeRepeatsMax
      repMin            <- optMaybeRepeatsMin
      group             <- validateRepeatsAndBuildGroup(repMax, repMin, fieldValues)
    } yield group
  }

  private def validateRepeatsAndBuildGroup(
    repMax: Option[Int],
    repMin: Option[Int],
    fields: List[FormComponent]
  ): Either[UnexpectedState, Group] =
    (repMax, repMin) match {
      case (Some(repMaxV), Some(repMinV)) if repMaxV < repMinV =>
        UnexpectedState(s"""repeatsMax should be higher than repeatsMin in Group field""").asLeft
      case (Some(repMaxV), Some(repMinV)) if repMinV < 0 =>
        UnexpectedState(s"""repeatsMin in Group field cannot be a negative number""").asLeft
      case (_, repeatsMin) =>
        val fieldsMandatory =
          if (repeatsMin.getOrElse(0) === 0) fields.map(field => field.copy(mandatory = false)) else fields
        Group(fieldsMandatory, repMax, repMin, repeatLabel, repeatAddAnotherText).asRight
    }

  private def toSmartString(stringEn: String, stringCy: String) =
    SmartString(LocalisedString(Map(LangADT.En -> stringEn, LangADT.Cy -> stringCy)), Nil)

  private lazy val choiceOpt: Opt[Choice] = {
    val yesNo =
      NonEmptyList
        .of(toSmartString("Yes", "Iawn"), toSmartString("No", "Na"))
        .map(OptionData.IndexBased(_, None, None, None, None))

    for {
      emailVerification        <- optEmailVerification
      selectionCriteria        <- optSelectionCriteria
      maybeFormatExpr          <- optMaybeFormatExpr(roundingMode)(selectionCriteria)(emailVerification)
      maybeValueExpr           <- optMaybeValueExpr
      choices                  <- choicesOpt
      multivalue               <- optMultivalue
      maybeHideChoicesSelected <- optHideChoicesSelected
      hideChoicesSelected = maybeHideChoicesSelected.getOrElse(false)
      oChoice: Opt[Choice] = (maybeFormatExpr, choices, multivalue, maybeValueExpr) match {
                               // format: off
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) => Choice(Checkbox, NonEmptyList(x, xs),          Vertical,   selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        case (IsOrientation(VerticalOrientation),   Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(Radio,    NonEmptyList(x, xs),          Vertical,   selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueYes), Selections(selections)) => Choice(Checkbox, NonEmptyList(x, xs),          Horizontal, selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        case (IsOrientation(HorizontalOrientation), Some(x :: xs), IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(Radio,    NonEmptyList(x, xs),          Horizontal, selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        case (IsOrientation(YesNoOrientation),      None,          IsMultivalue(MultivalueNo),  Selections(selections)) => Choice(YesNo,    yesNo, Horizontal, selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        case (IsOrientation(YesNoOrientation),      _,             _,                           Selections(selections)) => Choice(YesNo,    yesNo, Horizontal, selections, optionHints, optionHelpText, dividerPositon, dividerText, noneChoice, noneChoiceError, hideChoicesSelected).asRight
        // format: on
                               case (invalidFormat, invalidChoices, invalidMultivalue, invalidValue) =>
                                 UnexpectedState(
                                   s"""|Unsupported combination of 'format, choices, multivalue and value':
                                       |Format     : $invalidFormat
                                       |Choices    : $invalidChoices
                                       |Multivalue : $invalidMultivalue
                                       |Value      : $invalidValue
                                       |optionHints: $optionHints
                                       |optionHelpText: $optionHelpText
                                       |""".stripMargin
                                 ).asLeft
                             }
      result <- oChoice
    } yield result
  }

  private lazy val revealingChoiceOpt: Opt[RevealingChoice] = for {
    maybeValueExpr <- optMaybeValueExpr
    result         <- createRevealingChoice(maybeValueExpr)
  } yield result

  private def createRevealingChoice(maybeValueExpr: Option[ValueExpr]): Opt[RevealingChoice] = {

    val maybeRevealingFields: Option[Opt[List[List[FormComponent]]]] =
      revealingFields.map(_.traverse(_.traverse(_.optFieldValue())))

    choicesOpt.flatMap { choices =>
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
              optionHints.fold[List[Option[SmartString]]](List.fill(revealingFields.size)(None))(
                _.toList.map(hint => if (hint.allNonEmpty) Some(hint) else None)
              )

            val revealingChoiceElements: List[RevealingChoiceElement] =
              options
                .zip(revealingFields)
                .zip(selections)
                .zip(hints)
                .map { case (((choice, fields), selected), hint) =>
                  RevealingChoiceElement(choice, fields, hint, selected)
                }

            (revealingChoiceElements, optMultivalue) match {
              case (x :: xs, Right(IsMultivalue(MultivalueYes))) => RevealingChoice(NonEmptyList(x, xs), true).asRight
              case (x :: xs, Right(IsMultivalue(MultivalueNo)))  => RevealingChoice(NonEmptyList(x, xs), false).asRight
              case (_ :: _, _) =>
                mkError(s"'$optMultivalue' is not a valid value for the revealing choice multivalue property")
              case _ => mkError("At least one choice needs to be specified")
            }
          }

          def construcRevealingChoice(maybeSelection: Option[Int]): Either[String, RevealingChoice] =
            if (optionHints.fold(false)(_.size =!= options.size)) {
              mkError(
                s"Number of 'choices': ${options.size} and number of 'hints': ${optionHints.map(_.size)} does not match. They need to be identical."
              )
            } else if (options.size =!= revealingFields.size) {
              mkError(
                s"Number of 'choices': ${options.size} and number of 'revealingFields': ${revealingFields.size} does not match. They need to be identical."
              )
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
                              |choices : $choices
                              |selections: $maybeValueExpr
                              |revealingFields: $maybeRevealingFields""".stripMargin).asLeft
      }
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

  private lazy val fileUploadOpt: Opt[FileUpload] = for {
    maybeFileSizeLimit    <- toOpt((json \ "fileSizeLimit").validateOpt[Int], "/fileSizeLimit")
    maybeAllowedFileTypes <- toOpt((json \ "allowedFileTypes").validateOpt[AllowedFileTypes], "/allowedFileTypes")
  } yield FileUpload(maybeFileSizeLimit, maybeAllowedFileTypes)

  lazy val minFiles: Option[TextExpression] =
    (json \ "minFiles").toOption.flatMap {
      case JsNumber(number) => Some(TextExpression(Constant(number.toString)))
      case _                => (json \ "minFiles").validate[TextExpression].asOpt
    }

  lazy val maxFiles: Option[TextExpression] =
    (json \ "maxFiles").toOption.flatMap {
      case JsNumber(number) => Some(TextExpression(Constant(number.toString)))
      case _                => (json \ "maxFiles").validate[TextExpression].asOpt
    }

  private val multiFileUploadOpt: Opt[MultiFileUpload] = {
    for {
      maybeFileSizeLimit      <- toOpt((json \ "fileSizeLimit").validateOpt[Int], "/fileSizeLimit")
      maybeAllowedFileTypes   <- toOpt((json \ "allowedFileTypes").validateOpt[AllowedFileTypes], "/allowedFileTypes")
      maybeHint               <- toOpt((json \ "hint").validateOpt[SmartString], "/hint")
      maybeUploadAnotherLabel <- toOpt((json \ "uploadAnotherLabel").validateOpt[SmartString], "/uploadAnotherLabel")
      maybeContinueText       <- toOpt((json \ "continueText").validateOpt[SmartString], "/continueText")
    } yield MultiFileUpload(
      maybeFileSizeLimit,
      maybeAllowedFileTypes,
      maybeHint,
      maybeUploadAnotherLabel,
      maybeContinueText,
      minFiles.map(_.expr),
      maxFiles.map(_.expr)
    )
  }

  private lazy val infoOpt: Opt[InformationMessage] =
    for {
      infoText          <- optInfoText
      maybeSummaryValue <- toOpt((json \ "summaryValue").validateOpt[SmartString], "/summaryValue")
      informationMessage <- infoType match {
                              case IsInfoType(iType) => InformationMessage(iType, infoText, maybeSummaryValue).asRight
                              case _ =>
                                UnexpectedState(
                                  s"$infoType is invalid value of infoType. infoType must be one of: standard, long, important, banner or noformat"
                                ).asLeft
                            }
    } yield informationMessage

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

    val valueOpt = for {
      maybeValue <- toOpt((json \ "value").validateOpt[TextExpression], "/value")
    } yield maybeValue.map(_.expr)

    import OverseasAddress.Configurable._
    for {
      mandatoryLine2    <- optMandatoryLine2
      mandatoryCity     <- optMandatoryCity
      mandatoryPostcode <- optMandatoryPostcode
      countryLookup     <- optCountryLookup
      countryDisplayed  <- optCountryDisplayed
      line2             <- OverseasAddressParser.mandatoryField(mandatoryLine2, Mandatory.Line2)
      city              <- OverseasAddressParser.optionalField(mandatoryCity, Optional.City)
      postcode          <- OverseasAddressParser.mandatoryField(mandatoryPostcode, Mandatory.Postcode)
      value             <- valueOpt
      selectionCriteria <- optSelectionCriteria
    } yield {
      val mandatoryFields: List[Mandatory] = List(line2, postcode).flatten
      val optionalFields: List[Optional] = List(city).flatten

      OverseasAddress(
        mandatoryFields,
        optionalFields,
        countryLookup.getOrElse(true),
        value,
        countryDisplayed.getOrElse(true),
        selectionCriteria
      )
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
    def unapply(multivalue: Option[Boolean]): Option[Multivalue] =
      multivalue match {
        case Some(false) | None => Some(MultivalueNo)
        case Some(true)         => Some(MultivalueYes)
        case _                  => None
      }
  }

  private sealed trait International

  private final case object InternationalYes extends International
  private final case object InternationalNo extends International

  private final object IsInternational {
    def unapply(international: Option[Boolean]): Option[International] =
      international match {
        case Some(false) | None => Some(InternationalNo)
        case Some(true)         => Some(InternationalYes)
        case _                  => None
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
    def unapply(ignoredArgs: (Option[String], Opt[Option[Boolean]], Opt[Option[ValueExpr]])): Boolean =
      `type`.contains(InfoRaw)
  }

  private def parse[T: Reads, R](path: String, validate: T => Opt[R]): Opt[Option[R]] = {
    val optMaybeString: Opt[Option[T]] = toOpt((json \ path).validateOpt[T], path)
    import cats.implicits._
    for {
      maybeString <- optMaybeString
      res         <- maybeString.traverse[Opt, R](validate)

    } yield res
  }

  private def getTimeRange(json: JsValue): Range = {
    val formatter = DateTimeFormatter.ofPattern("HH:mm")
    val startTime = (json \ "startTime").as[String]
    val endTime = (json \ "endTime").as[String]
    Range(
      StartTime(Range.stringToLocalTime(formatter, startTime)),
      EndTime(Range.stringToLocalTime(formatter, endTime))
    )
  }

}
