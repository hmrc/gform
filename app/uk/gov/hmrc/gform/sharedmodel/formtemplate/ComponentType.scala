/*
 * Copyright 2022 HM Revenue & Customs
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

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import cats.Eq
import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService.{ IsFalseish, IsTrueish }
import uk.gov.hmrc.gform.sharedmodel.{ LocalisedString, SmartString, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ FieldName, RoboticsXml, StructuredFormDataFieldNamePurpose }

sealed trait ComponentType

case class Text(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  toUpperCase: UpperCaseBoolean = IsNotUpperCase,
  prefix: Option[SmartString] = None,
  suffix: Option[SmartString] = None
) extends ComponentType

sealed trait UpperCaseBoolean

case object IsUpperCase extends UpperCaseBoolean
case object IsNotUpperCase extends UpperCaseBoolean

object UpperCaseBoolean {
  private val templateReads: Reads[UpperCaseBoolean] = Reads {
    case JsString(IsTrueish())  => JsSuccess(IsUpperCase)
    case JsString(IsFalseish()) => JsSuccess(IsNotUpperCase)
    case invalid                => JsError("toUpperCase needs to be 'true' or 'false', got " + invalid)
  }
  implicit val format: OFormat[UpperCaseBoolean] = OFormatWithTemplateReadFallback(templateReads)
}

case class TextArea(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  rows: Int = TextArea.defaultRows,
  displayCharCount: Boolean = TextArea.defaultDisplayCharCount
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
case object PostcodeLookup extends ComponentType

case object TaxPeriodDate extends ComponentType

case class Address(international: Boolean) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = Address.fields(id)

  def alternateNamesFor(fcId: FormComponentId): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    Map(RoboticsXml -> FieldName(fcId.value.replace("street", "line")))

}

object Address {
  val mandatoryFields: FormComponentId => List[FormComponentId] = id => List("street1").map(id.withSuffix)
  val optionalFields: FormComponentId => List[FormComponentId] = id =>
    List("street2", "street3", "street4", "uk", "postcode", "country").map(id.withSuffix)
  val fields: FormComponentId => NonEmptyList[FormComponentId] = id =>
    NonEmptyList.fromListUnsafe(mandatoryFields(id) ++ optionalFields(id))
}

case class OverseasAddress(
  mandatoryFields: List[OverseasAddress.Configurable.Mandatory],
  optionalFields: List[OverseasAddress.Configurable.Optional],
  value: Option[OverseasAddress.Value],
  countryLookup: Boolean
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

  case class Value(
    line1: SmartString,
    line2: SmartString,
    line3: SmartString,
    city: SmartString,
    postcode: SmartString,
    country: SmartString
  )

  object Value {
    implicit val format: OFormat[Value] = derived.oformat()

    implicit val leafExprs: LeafExpr[Value] = (path: TemplatePath, t: Value) =>
      LeafExpr(path + "line1", t.line1) ++
        LeafExpr(path + "line2", t.line2) ++
        LeafExpr(path + "line3", t.line3) ++
        LeafExpr(path + "city", t.city) ++
        LeafExpr(path + "postcode", t.postcode) ++
        LeafExpr(path + "country", t.country)

  }

  implicit val format: OFormat[OverseasAddress] = derived.oformat()

}

object DisplayWidth extends Enumeration {
  type DisplayWidth = Value
  val XS, S, M, L, XL, XXL, DEFAULT = Value

  implicit val displayWidthReads: Reads[DisplayWidth] = Reads.enumNameReads(DisplayWidth)
  implicit val displayWidthWrites: Writes[DisplayWidth] = Writes.enumNameWrites
}

object SummaryDisplayWidth extends Enumeration {
  type SummaryDisplayWidth = Value
  val M, L, XL = Value

  implicit val displayWidthReads: Reads[SummaryDisplayWidth] =
    Reads.enumNameReads(SummaryDisplayWidth).preprocess {
      case JsString(s) => JsString(s.toUpperCase)
      case o           => o
    }
  implicit val displayWidthWrites: Writes[SummaryDisplayWidth] = Writes.enumNameWrites
}

sealed trait OptionData extends Product with Serializable

object OptionData {

  case class IndexBased(
    label: SmartString,
    includeIf: Option[IncludeIf]
  ) extends OptionData

  case class ValueBased(
    label: SmartString,
    value: String,
    includeIf: Option[IncludeIf]
  ) extends OptionData

  private val templateReads: Reads[OptionData] = {

    val indexBasedReads: Reads[OptionData] = Json.reads[IndexBased].widen[OptionData]
    val valueBasedReads: Reads[OptionData] = Json.reads[ValueBased].widen[OptionData]

    valueBasedReads | indexBasedReads
  }

  implicit val format: OFormat[OptionData] = OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[OptionData] = (path: TemplatePath, t: OptionData) =>
    t match {
      case OptionData.IndexBased(label, includeIf) =>
        LeafExpr(path + "label", label) ++ LeafExpr(path + "includeIf", includeIf)
      case OptionData.ValueBased(label, _, includeIf) =>
        LeafExpr(path + "label", label) ++ LeafExpr(path + "includeIf", includeIf)
    }
}

sealed trait NoneChoice extends Product with Serializable

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

case class Choice(
  `type`: ChoiceType,
  options: NonEmptyList[OptionData],
  orientation: Orientation,
  selections: List[Int],
  hints: Option[NonEmptyList[SmartString]],
  optionHelpText: Option[NonEmptyList[SmartString]],
  dividerPosition: Option[Int],
  dividerText: LocalisedString,
  noneChoice: Option[NoneChoice],
  noneChoiceError: Option[LocalisedString]
) extends ComponentType

sealed trait ChoiceType
final case object Radio extends ChoiceType
final case object Checkbox extends ChoiceType
final case object YesNo extends ChoiceType

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

case class InformationMessage(infoType: InfoType, infoText: SmartString) extends ComponentType

sealed trait FileUploadProvider

object FileUploadProvider {
  final case class Upscan(compression: Boolean) extends FileUploadProvider
  case object FileUploadFrontend extends FileUploadProvider

  implicit val format: OFormat[FileUploadProvider] = derived.oformat()
}

case class FileUpload(
  fileUploadProvider: FileUploadProvider
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
    includeIf: Option[IncludeIf]
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
      case t: HeaderRow => LeafExpr(path + "key", t.header)
      case t: ATLRow =>
        LeafExpr(path + "key", t.atlId) ++ LeafExpr(path + "key", t.includeIf) ++ LeafExpr(path + "key", t.rows)
    }
  implicit val format: Format[MiniSummaryRow] = derived.oformat()
}

case class MiniSummaryList(rows: List[MiniSummaryRow]) extends ComponentType
object MiniSummaryList {
  implicit val format: Format[MiniSummaryList] = derived.oformat()
}

case class TableValue(value: SmartString, cssClass: Option[String], colspan: Option[Int])

object TableValue {
  implicit val leafExprs: LeafExpr[TableValue] = (path: TemplatePath, r: TableValue) =>
    LeafExpr(path + "value", r.value)
  implicit val format: Format[TableValue] = derived.oformat()
}

case class TableValueRow(
  values: List[TableValue],
  includeIf: Option[IncludeIf]
)

object TableValueRow {
  implicit val leafExprs: LeafExpr[TableValueRow] = (path: TemplatePath, r: TableValueRow) =>
    LeafExpr(path + "includeIf", r.includeIf) ++ LeafExpr(path + "values", r.values)
  implicit val format: Format[TableValueRow] = derived.oformat()
}

case class TableComp(
  header: List[SmartString],
  rows: List[TableValueRow],
  caption: Option[String] = None,
  captionClasses: String = "",
  classes: String = "",
  firstCellIsHeader: Boolean = false
) extends ComponentType

object TableComp {
  implicit val format: Format[TableComp] = derived.oformat()
}

object ComponentType {

  implicit def readsNonEmptyList[T: Reads] = Reads[NonEmptyList[T]] { json =>
    Json.fromJson[List[T]](json).flatMap {
      case Nil     => JsError(JsonValidationError(s"Required at least one element. Got: $json"))
      case x :: xs => JsSuccess(NonEmptyList(x, xs))
    }
  }

  implicit def writesNonEmptyList[T: Writes] = Writes[NonEmptyList[T]] { v =>
    JsArray((v.head :: v.tail).map(Json.toJson(_)))
  }
  implicit val format: OFormat[ComponentType] = derived.oformat()

  implicit val leafExprs: LeafExpr[ComponentType] = (path: TemplatePath, t: ComponentType) =>
    t match {
      case Text(constraint, expr, _, _, prefix, suffix) =>
        ExprWithPath(path + "value", expr) ::
          LeafExpr(path + "prefix", prefix) ++
          LeafExpr(path + "suffix", suffix) ++
          LeafExpr(path + "format", constraint)
      case TextArea(constraint, expr, _, _, _) => ExprWithPath(path, expr) :: LeafExpr(path, constraint)
      case Date(_, _, _)                       => Nil
      case CalendarDate                        => Nil
      case PostcodeLookup                      => Nil
      case TaxPeriodDate                       => Nil
      case Address(_)                          => Nil
      case OverseasAddress(_, _, value, _)     => LeafExpr(path, value)
      case Choice(_, options, _, _, hints, optionHelpText, _, _, _, _) =>
        LeafExpr(path + "choices", options) ++
          LeafExpr(path + "hints", hints) ++
          LeafExpr(path + "optionHelpText", optionHelpText)
      case RevealingChoice(options, _)   => LeafExpr(path, options)
      case HmrcTaxPeriod(_, idNumber, _) => List(ExprWithPath(path, idNumber))
      case Group(fields, _, _, repeatLabel, addAnotherText) =>
        LeafExpr(path, fields) ++
          LeafExpr(path + "repeatLabel", repeatLabel) ++
          LeafExpr(path + "repeatAddAnotherText", addAnotherText)
      case InformationMessage(_, infoText) => LeafExpr(path + "infoText", infoText)
      case FileUpload(_)                   => Nil
      case Time(_, _)                      => Nil
      case MiniSummaryList(rows)           => LeafExpr(path + "rows", rows)
      case t: TableComp                    => LeafExpr(path + "header", t.header) ++ LeafExpr(path + "rows", t.rows)
    }

}
