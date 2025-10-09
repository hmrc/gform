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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Eq
import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.formtemplate.TableHeaderCellMaker
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.KeyDisplayWidth.KeyDisplayWidth
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ FieldName, RoboticsXml, StructuredFormDataFieldNamePurpose }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString, ValueClassFormat }

import java.time.LocalTime
import java.time.format.DateTimeFormatter

sealed trait ComponentType

case class Text(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  toUpperCase: UpperCaseBoolean = IsNotUpperCase,
  prefix: Option[SmartString] = None,
  suffix: Option[SmartString] = None,
  priority: Option[Priority] = None
) extends ComponentType

sealed trait UpperCaseBoolean

case object IsUpperCase extends UpperCaseBoolean
case object IsNotUpperCase extends UpperCaseBoolean

object UpperCaseBoolean {
  private val templateReads: Reads[UpperCaseBoolean] = Reads {
    case JsTrue  => JsSuccess(IsUpperCase)
    case JsFalse => JsSuccess(IsNotUpperCase)
    case invalid => JsError("toUpperCase needs to be 'true' or 'false', got " + invalid)
  }
  implicit val format: OFormat[UpperCaseBoolean] = OFormatWithTemplateReadFallback(templateReads)
}

case class TextArea(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  rows: Int = TextArea.defaultRows,
  displayCharCount: Boolean = TextArea.defaultDisplayCharCount,
  dataThreshold: Option[Int]
) extends ComponentType

object TextArea {
  val defaultRows = 5
  val defaultDisplayCharCount = true
}

case class Date(
  constraintType: DateConstraintType,
  offset: Offset,
  value: Option[DateValue]
) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = Date.fields(id)
}

object Date {
  val fields: FormComponentId => NonEmptyList[FormComponentId] = (id: FormComponentId) =>
    NonEmptyList.of("day", "month", "year").map(id.withSuffix)
}

case object CalendarDate extends ComponentType
case class PostcodeLookup(
  chooseAddressLabel: Option[SmartString],
  confirmAddressLabel: Option[SmartString],
  enterAddressLabel: Option[SmartString]
) extends ComponentType

object PostcodeLookup {
  implicit val format: OFormat[PostcodeLookup] = derived.oformat()
}

case object TaxPeriodDate extends ComponentType

case class Address(
  international: Boolean,
  mandatoryFields: List[Address.Configurable.Mandatory],
  countyDisplayed: Boolean,
  value: Option[Expr]
) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = Address.fields(id)

  def alternateNamesFor(fcId: FormComponentId): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    Map(RoboticsXml -> FieldName(fcId.value.replace("street", "line")))

}

object Address {
  object Configurable {
    sealed trait Mandatory
    object Mandatory {
      case object City extends Mandatory
      implicit val format: OFormat[Mandatory] = derived.oformat()
    }
  }

  val mandatoryFields: FormComponentId => List[FormComponentId] = id => List("street1").map(id.withSuffix)
  val optionalFields: FormComponentId => List[FormComponentId] = id =>
    List("street2", "street3", "street4", "uk", "postcode", "country").map(id.withSuffix)
  val fields: FormComponentId => NonEmptyList[FormComponentId] = id =>
    NonEmptyList.fromListUnsafe(mandatoryFields(id) ++ optionalFields(id))
}

case class OverseasAddress(
  mandatoryFields: List[OverseasAddress.Configurable.Mandatory],
  optionalFields: List[OverseasAddress.Configurable.Optional],
  countryLookup: Boolean,
  value: Option[Expr],
  countryDisplayed: Boolean,
  selectionCriteria: Option[List[SelectionCriteria]] = None
) extends ComponentType

object OverseasAddress {

  object Configurable {
    sealed trait Mandatory
    object Mandatory {
      case object Line2 extends Mandatory
      case object Postcode extends Mandatory
      implicit val format: OFormat[Mandatory] = derived.oformat()
    }

    sealed trait Optional
    object Optional {
      case object City extends Optional
      implicit val format: OFormat[Optional] = derived.oformat()
    }
  }

  implicit val format: OFormat[OverseasAddress] = derived.oformat()

}

object DisplayWidth extends Enumeration {
  type DisplayWidth = Value
  val XS, S, M, L, XL, XXL, DEFAULT = Value

  implicit val displayWidthReads: Reads[DisplayWidth] = Reads.enumNameReads(DisplayWidth)
  implicit val displayWidthWrites: Writes[DisplayWidth] = Writes.enumNameWrites
}

object KeyDisplayWidth extends Enumeration {
  type KeyDisplayWidth = Value
  val S, M, L = Value

  implicit val keyDisplayWidthReads: Reads[KeyDisplayWidth] = Reads.enumNameReads(KeyDisplayWidth).preprocess {
    case JsString(s) => JsString(s.toUpperCase)
    case o           => o
  }
  implicit val keyDisplayWidthWrites: Writes[KeyDisplayWidth] = Writes.enumNameWrites
}

object LayoutDisplayWidth extends Enumeration {
  type LayoutDisplayWidth = Value
  val M, L, XL = Value

  implicit val displayWidthReads: Reads[LayoutDisplayWidth] =
    Reads.enumNameReads(LayoutDisplayWidth).preprocess {
      case JsString(s) => JsString(s.toUpperCase)
      case o           => o
    }
  implicit val displayWidthWrites: Writes[LayoutDisplayWidth] = Writes.enumNameWrites
}

sealed trait Dynamic extends Product with Serializable

object Dynamic {

  final case class ATLBased(formComponentId: FormComponentId) extends Dynamic
  final case class DataRetrieveBased(indexOfDataRetrieveCtx: IndexOfDataRetrieveCtx) extends Dynamic

  val templateReads: Reads[Dynamic] = Reads.StringReads.flatMap { d =>
    val parsed = ValueParser.validate("${" + d + "}")
    parsed match {
      case Right(TextExpression(FormCtx(fcId))) => Reads.pure(Dynamic.ATLBased(fcId))
      case Right(TextExpression(drc @ DataRetrieveCtx(_, _))) =>
        Reads.pure(Dynamic.DataRetrieveBased(IndexOfDataRetrieveCtx(drc, 0)))
      case _ => Reads.failed("Wrong expression used in dynamic: " + d)
    }
  }

  implicit val dataRetrieveCtx: Format[DataRetrieveCtx] = derived.oformat()
  implicit val indexOfDataRetrieveCtx: Format[IndexOfDataRetrieveCtx] = derived.oformat()
  implicit val format: Format[Dynamic] = OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[Dynamic] = (path: TemplatePath, t: Dynamic) =>
    t match {
      case Dynamic.ATLBased(formComponentId)          => LeafExpr(path, formComponentId)
      case Dynamic.DataRetrieveBased(dataRetrieveCtx) => LeafExpr(path, dataRetrieveCtx)
    }
}

sealed trait OptionData extends Product with Serializable

object OptionData {

  case class IndexBased(
    label: SmartString,
    hint: Option[SmartString],
    includeIf: Option[IncludeIf],
    dynamic: Option[Dynamic],
    summaryValue: Option[SmartString]
  ) extends OptionData

  case class ValueBased(
    label: SmartString,
    hint: Option[SmartString],
    includeIf: Option[IncludeIf],
    dynamic: Option[Dynamic],
    value: OptionDataValue,
    summaryValue: Option[SmartString],
    keyWord: Option[SmartString]
  ) extends OptionData

  private val templateReads: Reads[OptionData] = {
    val hasValueResult = Reads(json => optionDataContainsValue(json))
    hasValueResult.flatMap { hasValue =>
      if (hasValue) {
        Json.reads[ValueBased].widen[OptionData]
      } else {
        Json.reads[IndexBased].widen[OptionData]
      }
    }
  }

  private def optionDataContainsValue(json: JsValue): JsResult[Boolean] =
    json.validate[String]((JsPath \ "value").read[String]) match {
      case JsSuccess(_, _) => JsSuccess(true)
      case _               => JsSuccess(false)
    }

  implicit val format: OFormat[OptionData] = OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[OptionData] = (path: TemplatePath, t: OptionData) =>
    t match {
      case OptionData.IndexBased(label, hint, includeIf, dynamic, summaryValue) =>
        LeafExpr(path + "label", label) ++
          LeafExpr(path + "hint", hint) ++
          LeafExpr(path + "includeIf", includeIf) ++
          LeafExpr(path + "dynamic", dynamic) ++
          LeafExpr(path + "summaryValue", summaryValue)
      case OptionData.ValueBased(label, hint, includeIf, dynamic, value, summaryValue, keyWord) =>
        LeafExpr(path + "label", label) ++
          LeafExpr(path + "hint", hint) ++
          LeafExpr(path + "includeIf", includeIf) ++
          LeafExpr(path + "dynamic", dynamic) ++
          LeafExpr(path + "value", value) ++
          LeafExpr(path + "summaryValue", summaryValue) ++
          LeafExpr(path + "keyWord", keyWord)
    }
}

sealed trait NoneChoice extends Product with Serializable {
  def fold[A](f: NoneChoice.IndexBased => A)(g: NoneChoice.ValueBased => A): A = this match {
    case i: NoneChoice.IndexBased => f(i)
    case v: NoneChoice.ValueBased => g(v)
  }
}

object NoneChoice {

  case class IndexBased(index: Int) extends NoneChoice
  case class ValueBased(value: String) extends NoneChoice

  private val templateReads: Reads[NoneChoice] = {

    val indexBasedReads: Reads[NoneChoice] = Reads.IntReads.map(IndexBased.apply).widen[NoneChoice]
    val valueBasedReads: Reads[NoneChoice] = Reads.StringReads.map(ValueBased.apply).widen[NoneChoice]

    valueBasedReads | indexBasedReads
  }

  implicit val format: OFormat[NoneChoice] = OFormatWithTemplateReadFallback(templateReads)
}

sealed trait DividerPosition extends Product with Serializable

object DividerPosition {
  final case class Number(pos: Int) extends DividerPosition
  final case class Value(value: String) extends DividerPosition
  implicit val format: OFormat[DividerPosition] = derived.oformat()
}

case class Choice(
  `type`: ChoiceType,
  options: NonEmptyList[OptionData],
  orientation: Orientation,
  selections: List[Int],
  hints: Option[NonEmptyList[SmartString]],
  optionHelpText: Option[NonEmptyList[SmartString]],
  dividerPosition: Option[DividerPosition],
  dividerText: LocalisedString,
  noneChoice: Option[NoneChoice],
  noneChoiceError: Option[LocalisedString],
  hideChoicesSelected: Boolean
) extends ComponentType

sealed trait ChoiceType
final case object Radio extends ChoiceType
final case object Checkbox extends ChoiceType
final case object YesNo extends ChoiceType
final case object TypeAhead extends ChoiceType

object ChoiceType {
  implicit val format: OFormat[ChoiceType] = derived.oformat()
  implicit val equal: Eq[ChoiceType] = Eq.fromUniversalEquals
}

case class RevealingChoiceElement(
  choice: OptionData,
  revealingFields: List[FormComponent],
  hint: Option[SmartString],
  selected: Boolean
)
object RevealingChoiceElement {
  implicit val format: OFormat[RevealingChoiceElement] = derived.oformat()

  implicit val leafExprs: LeafExpr[RevealingChoiceElement] = (path: TemplatePath, t: RevealingChoiceElement) =>
    LeafExpr(path + "choice", t.choice) ++
      LeafExpr(path + "revealingFields", t.revealingFields) ++
      LeafExpr(path + "hint", t.hint)

}
case class RevealingChoice(options: NonEmptyList[RevealingChoiceElement], multiValue: Boolean) extends ComponentType
object RevealingChoice {
  implicit val format: OFormat[RevealingChoice] = {
    import JsonUtils._
    derived.oformat()
  }
}

case class IdType(value: String) extends AnyVal
case class RegimeType(value: String) extends AnyVal

object IdType {
  implicit val format: OFormat[IdType] = ValueClassFormat.oformat("idType", IdType.apply, _.value)
}
object RegimeType {
  implicit val format: OFormat[RegimeType] = ValueClassFormat.oformat("regimeType", RegimeType.apply, _.value)
}

case class HmrcTaxPeriod(idType: IdType, idNumber: Expr, regimeType: RegimeType) extends ComponentType

object HmrcTaxPeriod {
  implicit val catsEq: Eq[HmrcTaxPeriod] = Eq.fromUniversalEquals
  implicit val format: OFormat[HmrcTaxPeriod] = derived.oformat()
}

sealed trait Orientation
case object Vertical extends Orientation
case object Horizontal extends Orientation
object Orientation {

  implicit val format: OFormat[Orientation] = derived.oformat()
}

sealed trait InfoType
case object StandardInfo extends InfoType

case object LongInfo extends InfoType

case object ImportantInfo extends InfoType

case object BannerInfo extends InfoType

case object NoFormat extends InfoType

object InfoType {
  implicit val format: OFormat[InfoType] = derived.oformat()
}

case class Group(
  fields: List[FormComponent],
  repeatsMax: Option[Int] = None,
  repeatsMin: Option[Int] = None,
  repeatLabel: Option[SmartString] = None,
  repeatAddAnotherText: Option[SmartString] = None
) extends ComponentType

case class InformationMessage(infoType: InfoType, infoText: SmartString, summaryValue: Option[SmartString] = None)
    extends ComponentType

case class FileUpload(
  fileSizeLimit: Option[Int],
  allowedFileTypes: Option[AllowedFileTypes]
) extends ComponentType

case class MultiFileUpload(
  fileSizeLimit: Option[Int],
  allowedFileTypes: Option[AllowedFileTypes],
  hint: Option[SmartString],
  uploadAnotherLabel: Option[SmartString],
  continueText: Option[SmartString],
  minFiles: Option[Expr],
  maxFiles: Option[Expr]
) extends ComponentType

case class StartTime(time: LocalTime) extends AnyVal

object StartTime {
  implicit val format: OFormat[StartTime] = derived.oformat()
}

case class EndTime(time: LocalTime) extends AnyVal

object EndTime {
  implicit val format: OFormat[EndTime] = derived.oformat()
}

case class Range(startTime: StartTime, endTime: EndTime)

object Range {
  implicit val format: OFormat[Range] = derived.oformat()

  def stringToLocalTime(formatter: DateTimeFormatter, time: String): LocalTime =
    LocalTime.parse(time, formatter)

  def localTimeToString(formatter: DateTimeFormatter, time: LocalTime): String =
    time.format(formatter)
}

case class IntervalMins(intervalMins: Int) extends AnyVal

object IntervalMins {
  implicit val format: OFormat[IntervalMins] = derived.oformat()
}

case class Time(ranges: List[Range], intervalMins: IntervalMins) extends ComponentType

object Time {
  implicit val format: OFormat[Time] = derived.oformat()
}

sealed trait MiniSummaryRow extends Product with Serializable

object MiniSummaryRow {
  case class ValueRow(
    key: Option[SmartString],
    value: MiniSummaryListValue,
    includeIf: Option[IncludeIf],
    pageId: Option[PageId],
    taskId: Option[TaskId]
  ) extends MiniSummaryRow

  case class SmartStringRow(
    key: Option[SmartString],
    value: SmartString,
    includeIf: Option[IncludeIf],
    pageId: Option[PageId],
    taskId: Option[TaskId]
  ) extends MiniSummaryRow

  case class HeaderRow(
    header: SmartString
  ) extends MiniSummaryRow

  case class ATLRow(
    atlId: FormComponentId,
    includeIf: Option[IncludeIf],
    rows: List[MiniSummaryRow]
  ) extends MiniSummaryRow

  implicit val leafExprs: LeafExpr[MiniSummaryRow] = (path: TemplatePath, r: MiniSummaryRow) =>
    r match {
      case t: ValueRow =>
        LeafExpr(path + "key", t.key) ++ LeafExpr(path + "value", t.value) ++ LeafExpr(path + "includeIf", t.includeIf)
      case s: SmartStringRow =>
        LeafExpr(path + "key", s.key) ++ LeafExpr(path + "value", s.value) ++ LeafExpr(path + "includeIf", s.includeIf)
      case t: HeaderRow => LeafExpr(path + "key", t.header)
      case t: ATLRow =>
        LeafExpr(path + "key", t.atlId) ++ LeafExpr(path + "key", t.includeIf) ++ LeafExpr(path + "key", t.rows)
    }
  implicit val format: Format[MiniSummaryRow] = derived.oformat()
}

case class MiniSummaryList(
  rows: List[MiniSummaryRow],
  displayInSummary: DisplayInSummary,
  keyDisplayWidth: Option[KeyDisplayWidth]
) extends ComponentType

object MiniSummaryList {
  implicit val format: Format[MiniSummaryList] = derived.oformat()
}

case class TableValue(value: SmartString, cssClass: Option[String], colspan: Option[Int], rowspan: Option[Int])

object TableValue {
  implicit val leafExprs: LeafExpr[TableValue] = (path: TemplatePath, r: TableValue) =>
    LeafExpr(path + "value", r.value)
  implicit val format: Format[TableValue] = derived.oformat()
}

case class TableValueRow(
  values: List[TableValue],
  includeIf: Option[IncludeIf],
  dynamic: Option[Dynamic]
)

object TableValueRow {
  implicit val leafExprs: LeafExpr[TableValueRow] = (path: TemplatePath, r: TableValueRow) =>
    LeafExpr(path + "includeIf", r.includeIf) ++ LeafExpr(path + "values", r.values) ++ LeafExpr(
      path + "dynamic",
      r.dynamic
    )
  implicit val format: Format[TableValueRow] = derived.oformat()
}

case class TableHeaderCell(
  label: SmartString,
  classes: Option[String]
)

object TableHeaderCell {
  implicit val leafExprs: LeafExpr[TableHeaderCell] = (path: TemplatePath, r: TableHeaderCell) =>
    LeafExpr(path + "label", r.label)

  private val tableHeaderCellReads: Reads[TableHeaderCell] = Reads(json =>
    new TableHeaderCellMaker(json)
      .optTableHeaderCell()
      .fold(us => JsError(us.toString), as => JsSuccess(as))
  )

  implicit val format: OFormat[TableHeaderCell] = OFormatWithTemplateReadFallback(tableHeaderCellReads)
}

case class TableComp(
  header: List[TableHeaderCell],
  rows: List[TableValueRow],
  summaryValue: SmartString,
  caption: Option[String] = None,
  captionClasses: String = "",
  classes: String = "",
  firstCellIsHeader: Boolean = false
) extends ComponentType

object TableComp {
  implicit val format: Format[TableComp] = derived.oformat()
}

case class Button(
  reference: Expr,
  amountInPence: Expr,
  isStartButton: Boolean,
  classes: Option[String]
) extends ComponentType

object Button {
  implicit val format: Format[Button] = derived.oformat()
}

object ComponentType {

  implicit def readsNonEmptyList[T: Reads]: Reads[NonEmptyList[T]] = Reads[NonEmptyList[T]] { json =>
    Json.fromJson[List[T]](json).flatMap {
      case Nil     => JsError(JsonValidationError(s"Required at least one element. Got: $json"))
      case x :: xs => JsSuccess(NonEmptyList(x, xs))
    }
  }

  implicit def writesNonEmptyList[T: Writes]: Writes[NonEmptyList[T]] = Writes[NonEmptyList[T]] { v =>
    JsArray((v.head :: v.tail).map(Json.toJson(_)))
  }
  implicit val format: OFormat[ComponentType] = derived.oformat()

  implicit val leafExprs: LeafExpr[ComponentType] = (path: TemplatePath, t: ComponentType) =>
    t match {
      case Text(constraint, expr, _, _, prefix, suffix, _) =>
        ExprWithPath(path + "value", expr) ::
          LeafExpr(path + "prefix", prefix) ++
          LeafExpr(path + "suffix", suffix) ++
          LeafExpr(path + "format", constraint)
      case TextArea(constraint, expr, _, _, _, _)     => ExprWithPath(path, expr) :: LeafExpr(path, constraint)
      case Date(_, _, _)                              => Nil
      case CalendarDate                               => Nil
      case PostcodeLookup(_, _, _)                    => Nil
      case TaxPeriodDate                              => Nil
      case Address(_, _, _, Some(expr))               => List(ExprWithPath(path, expr))
      case Address(_, _, _, _)                        => Nil
      case OverseasAddress(_, _, _, Some(expr), _, _) => List(ExprWithPath(path, expr))
      case OverseasAddress(_, _, _, _, _, _)          => Nil
      case Choice(_, options, _, _, hints, optionHelpText, _, _, _, _, _) =>
        LeafExpr(path + "choices", options) ++
          LeafExpr(path + "hints", hints) ++
          LeafExpr(path + "optionHelpText", optionHelpText)
      case RevealingChoice(options, _)   => LeafExpr(path, options)
      case HmrcTaxPeriod(_, idNumber, _) => List(ExprWithPath(path, idNumber))
      case Group(fields, _, _, repeatLabel, addAnotherText) =>
        LeafExpr(path, fields) ++
          LeafExpr(path + "repeatLabel", repeatLabel) ++
          LeafExpr(path + "repeatAddAnotherText", addAnotherText)
      case InformationMessage(_, infoText, summaryValue) =>
        LeafExpr(path + "infoText", infoText) ++ LeafExpr(path + "summaryValue", summaryValue)
      case FileUpload(_, _) => Nil
      case MultiFileUpload(_, _, hint, uploadAnotherLabel, continueText, minFiles, maxFiles) =>
        LeafExpr(path + "hint", hint) ++
          LeafExpr(path + "uploadAnotherLabel", uploadAnotherLabel) ++
          LeafExpr(path + "continueText", continueText) ++
          LeafExpr(path + "minFiles", minFiles) ++
          LeafExpr(path + "maxFiles", maxFiles)
      case Time(_, _)                  => Nil
      case MiniSummaryList(rows, _, _) => LeafExpr(path + "rows", rows)
      case t: TableComp =>
        LeafExpr(path + "header", t.header) ++ LeafExpr(path + "rows", t.rows) ++ LeafExpr(
          path + "summaryValue",
          t.summaryValue
        )
      case Button(reference, amountInPence, _, _) =>
        List(ExprWithPath(path, reference)) ++ List(ExprWithPath(path, amountInPence))
    }

}
