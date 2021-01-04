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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import java.time.LocalTime
import java.time.format.DateTimeFormatter

import cats.Eq
import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.formtemplate.FormComponentMakerService.{ IsFalseish, IsTrueish }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, ValueClassFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ FieldName, RoboticsXml, StructuredFormDataFieldNamePurpose }

import scala.collection.immutable._
sealed trait ComponentType

case class Text(
  constraint: TextConstraint,
  value: Expr,
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT,
  toUpperCase: UpperCaseBoolean = IsNotUpperCase
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
  displayWidth: DisplayWidth = DisplayWidth.DEFAULT
) extends ComponentType

case class UkSortCode(value: Expr) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = UkSortCode.fields(id)
}

object UkSortCode {
  val fields: FormComponentId => NonEmptyList[FormComponentId] = (id: FormComponentId) =>
    NonEmptyList.of("1", "2", "3").map(id.withSuffix)
}

case class Date(
  constraintType: DateConstraintType,
  offset: Offset,
  value: Option[DateValue]
) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = Date.fields(id)
}

case object Date {
  val fields: FormComponentId => NonEmptyList[FormComponentId] = (id: FormComponentId) =>
    NonEmptyList.of("day", "month", "year").map(id.withSuffix)
}

case class Address(international: Boolean) extends ComponentType {
  def fields(id: FormComponentId): NonEmptyList[FormComponentId] = Address.fields(id)

  def alternateNamesFor(fcId: FormComponentId): Map[StructuredFormDataFieldNamePurpose, FieldName] =
    Map(RoboticsXml -> FieldName(fcId.value.replace("street", "line")))

}

case object Address {
  val mandatoryFields: FormComponentId => List[FormComponentId] = id => List("street1").map(id.withSuffix)
  val optionalFields: FormComponentId => List[FormComponentId] = id =>
    List("street2", "street3", "street4", "uk", "postcode", "country").map(id.withSuffix)
  val fields: FormComponentId => NonEmptyList[FormComponentId] = id =>
    NonEmptyList.fromListUnsafe(mandatoryFields(id) ++ optionalFields(id))
}

object DisplayWidth extends Enumeration {
  type DisplayWidth = Value
  val XS, S, M, L, XL, XXL, DEFAULT = Value

  implicit val displayWidthReads: Reads[DisplayWidth] = Reads.enumNameReads(DisplayWidth)
  implicit val displayWidthWrites: Writes[DisplayWidth] = Writes.enumNameWrites
}

case class Choice(
  `type`: ChoiceType,
  options: NonEmptyList[SmartString],
  orientation: Orientation,
  selections: List[Int],
  optionHelpText: Option[NonEmptyList[SmartString]]
) extends ComponentType

sealed trait ChoiceType
final case object Radio extends ChoiceType
final case object Checkbox extends ChoiceType
final case object YesNo extends ChoiceType

object ChoiceType {
  implicit val format: OFormat[ChoiceType] = derived.oformat()
  implicit val equal: Eq[ChoiceType] = Eq.fromUniversalEquals
}

case class RevealingChoiceElement(choice: SmartString, revealingFields: List[FormComponent], selected: Boolean)
object RevealingChoiceElement {
  implicit val format: OFormat[RevealingChoiceElement] = derived.oformat()
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

case class FileUpload() extends ComponentType

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

}
