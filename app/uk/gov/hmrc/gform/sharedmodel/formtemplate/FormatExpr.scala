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
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.{ SelectionCriteriaParser, ValueParser }
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LocalisedString }

sealed trait FormatExpr
final case class OrientationFormat(value: String) extends FormatExpr
final case class DateFormat(expressions: DateConstraintType) extends FormatExpr
final case class TextFormat(number: TextConstraint) extends FormatExpr

sealed trait ValueExpr extends {
  def rewrite: ValueExpr = this match {
    case TextExpression(expr) => TextExpression(expr.rewrite)
    case otherwise            => otherwise
  }
}

final case class TextExpression(expr: Expr) extends ValueExpr
final case class DateExpression(dateValue: DateValue) extends ValueExpr
final case class ChoiceExpression(selections: List[Int]) extends ValueExpr

sealed trait DateConstraintType
final case object AnyDate extends DateConstraintType
final case class DateConstraints(constraints: List[DateConstraint]) extends DateConstraintType

object DateConstraintType {
  implicit val format: OFormat[DateConstraintType] = derived.oformat()
}

final case class DateConstraint(
  beforeAfterPrecisely: BeforeAfterPrecisely,
  dateFormat: DateConstraintInfo,
  offset: OffsetDate
)

object DateConstraint {
  implicit val format: OFormat[DateConstraint] = derived.oformat()
}

sealed trait BeforeAfterPrecisely
case object After extends BeforeAfterPrecisely
case object Before extends BeforeAfterPrecisely
case object Precisely extends BeforeAfterPrecisely

object BeforeAfterPrecisely {
  implicit val format: OFormat[BeforeAfterPrecisely] = derived.oformat()
}

sealed trait ExactParameter
sealed trait DateParameter

sealed trait Year extends DateParameter

object Year {
  implicit val format: OFormat[Year] = derived.oformat()

  case object Next extends Year with ExactParameter
  case object Previous extends Year with ExactParameter
  case object Any extends Year
  case class Exact(year: Int) extends Year with ExactParameter
}

sealed trait Month extends DateParameter

object Month {
  implicit val format: OFormat[Month] = derived.oformat()

  case object Any extends Month
  case class Exact(month: Int) extends Month with ExactParameter

}

sealed trait Day extends DateParameter

object Day {
  implicit val format: OFormat[Day] = derived.oformat()

  case object Any extends Day
  case class Exact(day: Int) extends Day with ExactParameter
  case object First extends Day with ExactParameter
  case object Last extends Day with ExactParameter
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo

case class ConcreteDate(year: Year, month: Month, day: Day) extends DateConstraintInfo

case class DateField(value: FormComponentId) extends DateConstraintInfo

object DateConstraintInfo {
  implicit val format: OFormat[DateConstraintInfo] = derived.oformat()
}

case class OffsetDate(value: Int) extends AnyVal

object OffsetDate {
  implicit val formatExpr: OFormat[OffsetDate] = Json.format[OffsetDate]
}

sealed trait RoundingMode

object RoundingMode {
  case object Up extends RoundingMode
  case object Down extends RoundingMode
  case object Ceiling extends RoundingMode
  case object Floor extends RoundingMode
  case object HalfEven extends RoundingMode
  case object HalfUp extends RoundingMode
  case object HalfDown extends RoundingMode

  val defaultRoundingMode: RoundingMode = Down

  implicit val format: Format[RoundingMode] = ADTFormat.formatEnumeration(
    "Up"       -> Up,
    "Down"     -> Down,
    "Ceiling"  -> Ceiling,
    "Floor"    -> Floor,
    "HalfDown" -> HalfDown,
    "HalfEven" -> HalfEven,
    "HalfUp"   -> HalfUp
  )
}

sealed trait TextConstraint

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFractionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None
) extends TextConstraint

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFractionalDigits,
  roundingMode: RoundingMode = RoundingMode.defaultRoundingMode,
  unit: Option[LocalisedString] = None
) extends TextConstraint

case class ShortText(min: Int, max: Int) extends TextConstraint
object ShortText { val default = ShortText(0, 1000) }
case class Lookup(register: Register, selectionCriteria: Option[List[SelectionCriteria]]) extends TextConstraint
case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint
case class Sterling(roundingMode: RoundingMode, positiveOnly: Boolean) extends TextConstraint
case class WholeSterling(positiveOnly: Boolean) extends TextConstraint
case class ReferenceNumber(min: Int, max: Int) extends TextConstraint
case object UkBankAccountNumber extends TextConstraint
case object UkSortCodeFormat extends TextConstraint
case object SubmissionRefFormat extends TextConstraint
case object YearFormat extends TextConstraint

case object TelephoneNumber extends TextConstraint {
  val minimumLength = 4
  val maximumLength = 25
  val phoneNumberValidation = """^[\+A-Z0-9 )/(*#-]+$""".r
}

case object Email extends TextConstraint
case class EmailVerifiedBy(formComponentId: FormComponentId, emailVerifierService: EmailVerifierService)
    extends TextConstraint
case object SaUTR extends TextConstraint
case object CtUTR extends TextConstraint
case object NINO extends TextConstraint
case object UkVrn extends TextConstraint
case object PayeReference extends TextConstraint
case object CountryCode extends TextConstraint
case object NonUkCountryCode extends TextConstraint
case object CompanyRegistrationNumber extends TextConstraint
case object EORI extends TextConstraint
case object UkEORI extends TextConstraint
case object ChildBenefitNumber extends TextConstraint

object TextConstraint {
  val default = TextWithRestrictions(0, 1000)
  val defaultWholeDigits = 11
  val defaultFractionalDigits = 2

  implicit val format: OFormat[TextConstraint] = derived.oformat()

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£' || c == ','))

  implicit def leafExprs: LeafExpr[TextConstraint] = (path: TemplatePath, t: TextConstraint) =>
    t match {
      case Lookup(register: Register, selectionCriteria: Option[List[SelectionCriteria]]) =>
        LeafExpr(path + "selectionCriteria", selectionCriteria)
      case _ => List.empty[ExprWithPath]
    }

}

sealed trait Register

object Register {
  case object AgentComplaintCategories extends Register
  case object CashType extends Register
  case object Country extends Register
  case object Currency extends Register
  case object Intent extends Register
  case object Intercept extends Register
  case object Origin extends Register
  case object Port extends Register
  case object TransportMode extends Register
  case object OriginWho extends Register
  case object OriginMainPart extends Register
  case object OriginSavingsEarnings extends Register
  case object OriginSellingSomething extends Register
  case object IntentBuyingWhat extends Register
  case object IntentBigPurchase extends Register
  case object IntentBusiness extends Register
  case object IntentOther extends Register
  case object IntentLivingCostsAndFees extends Register
  case object SicCode extends Register

  implicit val format: OFormat[Register] = derived.oformat()

}

case class CsvColumnName(column: String) extends AnyVal

object CsvColumnName {

  implicit val format: OFormat[CsvColumnName] = derived.oformat()
}

sealed trait SelectionCriteriaValue

object SelectionCriteriaValue {
  case class SelectionCriteriaExpr(expr: FormCtx) extends SelectionCriteriaValue
  case class SelectionCriteriaReference(expr: FormCtx, name: CsvColumnName) extends SelectionCriteriaValue
  case class SelectionCriteriaSimpleValue(value: List[String]) extends SelectionCriteriaValue
  private val reads: Reads[SelectionCriteriaValue] = Reads { json =>
    json \ "value" match {
      case JsDefined(JsArray(values)) =>
        JsSuccess(SelectionCriteriaSimpleValue(values.toList.map(_.as[String])))

      case JsDefined(JsString(value)) =>
        SelectionCriteriaParser.validate(value).fold(error => JsError(error.toString), JsSuccess(_))

      case JsDefined(unknown) =>
        JsError(s"Unexpected type $unknown for field 'value'. Expected types are Array and String.")

      case _: JsUndefined => JsError(s"Missing field 'value' in json $json")
    }
  }
  implicit val format: OFormat[SelectionCriteriaValue] = OFormatWithTemplateReadFallback(reads)

  implicit val leafExprs: LeafExpr[SelectionCriteriaValue] = (path: TemplatePath, t: SelectionCriteriaValue) =>
    t match {
      case SelectionCriteriaExpr(formCtx)         => List(ExprWithPath(path, formCtx))
      case SelectionCriteriaReference(formCtx, _) => List(ExprWithPath(path, formCtx))
      case SelectionCriteriaSimpleValue(_)        => Nil

    }
}

case class SelectionCriteria(column: CsvColumnName, value: SelectionCriteriaValue)

object SelectionCriteria {

  implicit val reads: Reads[SelectionCriteria] = Reads { json =>
    SelectionCriteriaValue.format.reads(json).flatMap { value =>
      json \ "column" match {
        case JsDefined(JsString(column)) => JsSuccess(SelectionCriteria(CsvColumnName(column), value))
        case JsDefined(unknown)          => JsError(s"Unexpected type $unknown for field 'column'. Expected type is String.")
        case _: JsUndefined              => JsError(s"Missing field 'column' in json $json")
      }
    }
  }

  implicit val format: OFormat[SelectionCriteria] = OFormatWithTemplateReadFallback(reads)

  implicit val leafExprs: LeafExpr[SelectionCriteria] = (path: TemplatePath, t: SelectionCriteria) =>
    implicitly[LeafExpr[SelectionCriteriaValue]].exprs(path + "value", t.value)

}

object TextExpression {

  //TODO: remove this logic from case class representing data.
  implicit val format: OFormat[TextExpression] = {
    val writes: OWrites[TextExpression] = derived.owrites()
    val stdReads = Json.reads[TextExpression]
    val reads: Reads[TextExpression] = stdReads orElse Reads {
      case JsString(expression) =>
        ValueParser
          .validateWithParser(expression, ValueParser.expr)
          .fold(unexpectedState => JsError(unexpectedState.toString), expr => JsSuccess(TextExpression(expr)))
      case otherwise => JsError(s"Expected String as JsValue for TextExpression, got: $otherwise")
    }
    OFormat[TextExpression](reads, writes)
  }

  implicit val equal: Eq[TextExpression] = Eq.fromUniversalEquals
}

sealed trait MiniSummaryListValue

object MiniSummaryListValue {
  case class AnyExpr(expr: Expr) extends MiniSummaryListValue
  case class Reference(expr: FormCtx) extends MiniSummaryListValue
  implicit val format: OFormat[MiniSummaryListValue] = derived.oformat()

  implicit val leafExprs: LeafExpr[MiniSummaryListValue] = (path: TemplatePath, t: MiniSummaryListValue) =>
    t match {
      case AnyExpr(exp)   => LeafExpr(path + "expr", exp)
      case Reference(exp) => LeafExpr(path + "expr", exp)
    }
}
