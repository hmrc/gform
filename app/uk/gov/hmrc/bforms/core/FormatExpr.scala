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

package uk.gov.hmrc.bforms.core

import julienrf.json.derived
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
 * Created by dimitra on 03/04/17.
 */

sealed trait BeforeOrAfter
case object After extends BeforeOrAfter
case object Before extends BeforeOrAfter

object BeforeOrAfter {
  implicit val formatExpr: OFormat[BeforeOrAfter] = derived.oformat[BeforeOrAfter]
}

sealed trait DateConstraintInfo
case object Today extends DateConstraintInfo
case class ConcreteDate(year: Int, month: Int, day: Int) extends DateConstraintInfo
case class NextDate(month: Int, day: Int) extends DateConstraintInfo
case class PreviousDate(month: Int, day: Int) extends DateConstraintInfo
case class AnyWord(value: String) extends DateConstraintInfo

object DateConstraintInfo {
  implicit val formatExpr: OFormat[DateConstraintInfo] = derived.oformat[DateConstraintInfo]
}

sealed trait OffsetFormat
case class OffsetDate(value: Int) extends OffsetFormat

object OffsetFormat {
  implicit val formatExpr: OFormat[OffsetFormat] = derived.oformat[OffsetFormat]
}

sealed trait FormatExpr
final case class TextExpression(value: String) extends FormatExpr
final case class DateFormat(expressions: DateConstraintType) extends FormatExpr

sealed trait DateConstraintType
final case object AnyDate extends DateConstraintType
final case class DateConstraints(constraints: List[DateConstraint]) extends DateConstraintType

object DateConstraintType {
  implicit val formatExpr: OFormat[DateConstraintType] = derived.oformat[DateConstraintType]
}

final case class DateConstraint(beforeOrAfter: BeforeOrAfter, dateFormat: DateConstraintInfo, offsetFormat: OffsetFormat)

object DateConstraint {
  implicit val formatExpr: OFormat[DateConstraint] = derived.oformat[DateConstraint]
}

object FormatExpr {
  implicit val format: OFormat[FormatExpr] = {
    val formatExpr: OFormat[FormatExpr] = derived.oformat[FormatExpr]

    val reads: Reads[FormatExpr] = (formatExpr: Reads[FormatExpr]) | Reads {
      case JsString(formatAsStr) =>
        FormatParser.validate(formatAsStr) match {
          case Right(expr) => JsSuccess(expr)
          case Left(error) => JsError(error.toString)
        }
      case otherwise => JsError(s"Invalid format expression. Expected String, got $otherwise")
    }

    OFormat[FormatExpr](reads, formatExpr)
  }
}
