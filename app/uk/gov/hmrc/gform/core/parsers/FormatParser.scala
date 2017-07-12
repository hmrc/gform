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

package uk.gov.hmrc.gform.core.parsers

import cats.instances.either._
import cats.syntax.either._
import parseback._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.models._
import BasicParsers._

object FormatParser {

  def validate(expression: String): Opt[FormatExpr] = validateWithParser(expression, expr)

  lazy val expr: Parser[FormatExpr] = (
    dateFormat
      | textFormat
      | anyWordExpression
    )

  lazy val dateFormat: Parser[DateFormat] = (
    anyDateConstraint ^^ ((loc, constraints) => DateFormat(constraints))
    | dateConstraints ^^ ((loc, constraints) => DateFormat(constraints))
  )

  lazy val dateConstraints: Parser[DateConstraints] = (
    dateConstraint ~ "," ~ dateConstraints ^^ { (loc, x, _, xs) => DateConstraints(x :: xs.constraints) }
    | dateConstraint ^^ { (loc, x) => DateConstraints(List(x)) }
  )

  lazy val dateConstraint: Parser[DateConstraint] = (
    beforeOrAfter ~
    dateExpr ~
    offsetExpression ^^ { (loc, beforeOrAfter, dateExpr, offset) => DateConstraint(beforeOrAfter, dateExpr, offset) }
  )

  lazy val anyDateConstraint: Parser[DateConstraintType] = (
    "anyDate" ^^ { (loc, _) => AnyDate }
  )

  lazy val concreteDate: Parser[ConcreteDate] = (
    yearParser ~ monthDay ^^ { (loc, year, month, day) => ConcreteDate(year, month, day) }
  )

  lazy val nextDate: Parser[NextDate] = nextOrPrevious("next", NextDate.apply)

  lazy val previousDate: Parser[PreviousDate] = nextOrPrevious("previous", PreviousDate.apply)

  lazy val beforeOrAfter: Parser[BeforeOrAfter] = (
    "after" ^^ { (loc, after) => After }
    | "before" ^^ { (loc, before) => Before }
  )

  lazy val dateExpr: Parser[DateConstraintInfo] = (
    "today" ^^ { (loc, today) => Today }
    | concreteDate
    | nextDate
    | previousDate
    | anyWordFormat ^^ { (loc, str) => AnyWord(str) }
  )

  lazy val anyWordExpression: Parser[FormatExpr] = (
    anyWordFormat ^^ { (loc, anyWord) => OrientationFormat(anyWord) }
  )

  lazy val offsetExpression: Parser[OffsetDate] = (
    anyInteger ^^ { (loc, offset) => OffsetDate(offset) }
  )

  lazy val textFormat: Parser[FormatExpr] = (
      numberFormat
        | positiveNumberFormat
        | positiveWholeNumberFormat
    )

  lazy val numberFormat: Parser[TextFormat] = (
    "number" ~ numberParameters ^^ { (loc, _, na) => TextFormat(Number(maxWholeDigits = na.maxWholeDigits, maxFractionalDigits = na.maxFractionalDigits, na.unit)) }
      | "number" ^^ { (loc, _) => TextFormat(Number()) }
    )

  lazy val positiveNumberFormat: Parser[TextFormat] = (
    "positiveNumber" ~ numberParameters ^^ { (loc, _, na) => TextFormat(PositiveNumber(maxWholeDigits = na.maxWholeDigits, maxFractionalDigits = na.maxFractionalDigits, na.unit)) }
      | "positiveNumber" ^^ { (loc, _) => TextFormat(PositiveNumber()) }
    )

  lazy val positiveWholeNumberFormat: Parser[TextFormat] = (
    "positiveWholeNumber" ^^ { (loc, _) => TextFormat(PositiveNumber(maxFractionalDigits = 0)) }
    )

  lazy val numberParameters: Parser[NumberArgs] = (
    "(" ~ numberIntParameters ~ "," ~ quotedString ~ ")" ^^ { (loc: List[Line], _, nia: NumberIntArgs, _, unit: UnitName, _) => NumberArgs(nia.maxWholeDigits, nia.maxFractionalDigits, unit.u) }
      | "(" ~ numberIntParameters ~ ")" ^^ { (loc: List[Line], _, nia: NumberIntArgs, _) => NumberArgs(nia.maxWholeDigits, nia.maxFractionalDigits) }
    )


  lazy val numberIntParameters: Parser[NumberIntArgs] = (
    positiveInteger ~ "," ~ positiveInteger ^^ { (loc: List[Line], whole: Int, _, fractional: Int) => NumberIntArgs(whole, fractional) }
    )


  case class NumberIntArgs(maxWholeDigits: Int = NumberFormat.defaultWholeDigits, maxFractionalDigits: Int = NumberFormat.defaultFactionalDigits)


  case class NumberArgs(maxWholeDigits: Int = NumberFormat.defaultWholeDigits, maxFractionalDigits: Int = NumberFormat.defaultFactionalDigits, unit: Option[String] = None)

  lazy val quotedString: Parser[UnitName] = (
    "'" ~ "[^']+".r ~ "'" ^^ { (loc, _, s, _) => UnitName(Some(s)) }
    )

  case class UnitName(u: Option[String])

}
