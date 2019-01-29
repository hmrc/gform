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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import java.time.LocalDate
import java.util.Calendar

import julienrf.json.derived
import org.joda.time.DateTime
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.{ BasicParsers, ValueParser }

sealed trait FormatExpr
final case class OrientationFormat(value: String) extends FormatExpr
final case class DateFormat(expressions: DateConstraintType) extends FormatExpr
final case class TextFormat(number: TextConstraint) extends FormatExpr

sealed trait ValueExpr

final case class TextExpression(expr: Expr) extends ValueExpr
final case class DateExpression(dateValue: DateValue) extends ValueExpr
final case class ChoiceExpression(selections: List[Int]) extends ValueExpr

sealed trait DateConstraintType
final case object AnyDate extends DateConstraintType
final case class DateConstraints(constraints: List[DateConstraint]) extends DateConstraintType

object DateConstraintType {
  implicit val format: OFormat[DateConstraintType] = derived.oformat[DateConstraintType]
}

final case class DateConstraint(
  beforeAfterPrecisely: BeforeAfterPrecisely,
  dateFormat: DateConstraintInfo,
  offset: OffsetDate
)

object DateConstraint {
  implicit val format: OFormat[DateConstraint] = derived.oformat[DateConstraint]
}

sealed trait BeforeAfterPrecisely
case object After extends BeforeAfterPrecisely
case object Before extends BeforeAfterPrecisely
case object Precisely extends BeforeAfterPrecisely

object BeforeAfterPrecisely {
  implicit val format: OFormat[BeforeAfterPrecisely] = derived.oformat[BeforeAfterPrecisely]
}

sealed trait Year
case object AnyYear extends Year
case class ExactYear(year: Int) extends Year

object Year {
  implicit val format: OFormat[Year] = derived.oformat[Year]
}

sealed trait Month
case object AnyMonth extends Month
case class ExactMonth(month: Int) extends Month

object Month {
  implicit val format: OFormat[Month] = derived.oformat[Month]
}

sealed trait Day
case object AnyDay extends Day
case class ExactDay(day: Int) extends Day
case object FirstDay extends Day
case object LastDay extends Day

object Day {
  implicit val format: OFormat[Day] = derived.oformat[Day]
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo
case class ConcreteDate(year: Year, month: Month, day: Day) extends DateConstraintInfo
case class NextDate(month: Month, day: Day) extends DateConstraintInfo
case class PreviousDate(month: Month, day: Day) extends DateConstraintInfo

//case class FirstDay(year: String, month: String) extends DateConstraintInfo {
//  val day = 1
//
//}
//case class LastDay(year: String, month: String) extends DateConstraintInfo {
//
//  private val isLeapYear = (year: Int) =>
//    ((year % 4) == 0) && !(((year % 100) == 0) &&
//      !((year % 400) == 0))
//
//  val day: Option[Int] = try {
//    Some(LocalDate.of(year.toInt, month.toInt, 1).getMonth.length(isLeapYear(year.toInt)))
//  } catch {
//    case _: NumberFormatException => None
//  }
//}
case class AnyWord(value: String) extends DateConstraintInfo
case class DateField(value: FormComponentId) extends DateConstraintInfo

object DateConstraintInfo {
  implicit val format: OFormat[DateConstraintInfo] = derived.oformat[DateConstraintInfo]
}

case class OffsetDate(value: Int) extends AnyVal

object OffsetDate {
  implicit val formatExpr: OFormat[OffsetDate] = Json.format[OffsetDate]
}

sealed trait TextConstraint

final case object AnyText extends TextConstraint

final case class Number(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  unit: Option[String] = None)
    extends TextConstraint

final case class PositiveNumber(
  maxWholeDigits: Int = TextConstraint.defaultWholeDigits,
  maxFractionalDigits: Int = TextConstraint.defaultFactionalDigits,
  unit: Option[String] = None)
    extends TextConstraint

case object BasicText extends TextConstraint
case object ShortText extends TextConstraint
case class TextWithRestrictions(min: Int, max: Int) extends TextConstraint
case object Sterling extends TextConstraint
case object UkBankAccountNumber extends TextConstraint
case object UkSortCodeFormat extends TextConstraint
case object UTR extends TextConstraint
case object NINO extends TextConstraint
case object TelephoneNumber extends TextConstraint
case object Email extends TextConstraint
case object UkVrn extends TextConstraint
case object CountryCode extends TextConstraint
case object NonUkCountryCode extends TextConstraint
case object CompanyRegistrationNumber extends TextConstraint
case object EORI extends TextConstraint

object TextConstraint {
  val defaultWholeDigits = 11
  val defaultFactionalDigits = 2

  implicit val format: OFormat[TextConstraint] = derived.oformat[TextConstraint]

  def filterNumberValue(s: String): String = s.filterNot(c => (c == '£' || c == ','))
}

object TextExpression {

  //TODO: remove this logic from case class representing data.
  implicit val format: OFormat[TextExpression] = {
    val writes: OWrites[TextExpression] = derived.owrites
    val stdReads = Json.reads[TextExpression]
    val reads: Reads[TextExpression] = stdReads orElse Reads {
      case JsString(expression) =>
        BasicParsers
          .validateWithParser(expression, ValueParser.expr)
          .fold(unexpectedState => JsError(unexpectedState.toString), expr => JsSuccess(TextExpression(expr)))
      case otherwise => JsError(s"Expected String as JsValue for TextExpression, got: $otherwise")
    }
    OFormat[TextExpression](reads, writes)
  }
}
