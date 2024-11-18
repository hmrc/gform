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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.models.constraints.ReferenceInfo
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

sealed trait DateExpr {

  def leafExprs: List[Expr] = this match {
    case DateValueExpr(_)              => DateCtx(this) :: Nil
    case DateFormCtxVar(formCtx)       => formCtx :: Nil
    case DateExprWithOffset(dExpr, _)  => dExpr.leafExprs
    case HmrcTaxPeriodCtx(formCtx, _)  => formCtx :: Nil
    case DataRetrieveDateCtx(_, _)     => DateCtx(this) :: Nil
    case DateIfElse(_, field1, field2) => field1.leafExprs ++ field2.leafExprs
    case DateOrElse(field1, field2)    => field1.leafExprs ++ field2.leafExprs
    case DateConstructExpr(dm, year)   => dm.leafExprs :+ year
  }

  def referenceInfos: List[ReferenceInfo] = this match {
    case DateValueExpr(_)              => Nil
    case DateFormCtxVar(formCtx)       => ReferenceInfo.FormCtxExpr(TemplatePath.leaf, formCtx) :: Nil
    case DateExprWithOffset(dExpr, _)  => dExpr.referenceInfos
    case HmrcTaxPeriodCtx(formCtx, _)  => ReferenceInfo.FormCtxExpr(TemplatePath.leaf, formCtx) :: Nil
    case DataRetrieveDateCtx(_, _)     => Nil
    case DateIfElse(_, field1, field2) => field1.referenceInfos ++ field2.referenceInfos
    case DateOrElse(field1, field2)    => field1.referenceInfos ++ field2.referenceInfos
    case DateConstructExpr(dm, year)   => dm.referenceInfos ++ ExprWithPath(TemplatePath.leaf, year).referenceInfos
  }

  def maybeFormCtx: List[FormCtx] = this match {
    case DateValueExpr(_)              => Nil
    case DateFormCtxVar(formCtx)       => List(formCtx)
    case DateExprWithOffset(dExpr, _)  => dExpr.maybeFormCtx
    case HmrcTaxPeriodCtx(formCtx, _)  => List(formCtx)
    case DataRetrieveDateCtx(_, _)     => Nil
    case DateIfElse(_, field1, field2) => field1.maybeFormCtx ++ field2.maybeFormCtx
    case DateOrElse(field1, field2)    => field1.maybeFormCtx ++ field2.maybeFormCtx
    case DateConstructExpr(_, _)       => Nil
  }
}

sealed trait OffsetUnit
object OffsetUnit {
  case class Day(n: Int) extends OffsetUnit
  case class Year(n: Int) extends OffsetUnit
  case class Month(n: Int) extends OffsetUnit

  implicit val format: OFormat[OffsetUnit] = derived.oformat()
}

case class OffsetYMD(offsets: List[OffsetUnit]) { // Order matters, since OffsetUnit is not commutative
  def +(that: OffsetYMD) = OffsetYMD(offsets ++ that.offsets)
}

object OffsetYMD {
  def apply(offsets: OffsetUnit*): OffsetYMD =
    OffsetYMD(offsets.toList)

  implicit val format: OFormat[OffsetYMD] = derived.oformat()
}

sealed trait DateExprValue
case object TodayDateExprValue extends DateExprValue
case class ExactDateExprValue(year: Int, month: Int, day: Int) extends DateExprValue

object DateExprValue {
  implicit val format: OFormat[DateExprValue] = derived.oformat()
}

case class DateValueExpr(value: DateExprValue) extends DateExpr
case class DateFormCtxVar(formCtx: FormCtx) extends DateExpr
case class DateExprWithOffset(dExpr: DateExpr, offset: OffsetYMD) extends DateExpr
case class HmrcTaxPeriodCtx(formCtx: FormCtx, hmrcTaxPeriodInfo: HmrcTaxPeriodInfo) extends DateExpr
case class DataRetrieveDateCtx(id: DataRetrieveId, attribute: DataRetrieve.Attribute) extends DateExpr
case class DateIfElse(ifElse: BooleanExpr, field1: DateExpr, field2: DateExpr) extends DateExpr
case class DateOrElse(field1: DateExpr, field2: DateExpr) extends DateExpr
case class DateConstructExpr(dayMonth: DateExpr, year: Expr) extends DateExpr

object DateExpr {
  implicit val format: OFormat[DateExpr] = derived.oformat()

  private def allFormCtxExprs(dateExpr: DateExpr): List[FormCtx] = dateExpr match {
    case DateValueExpr(_)              => Nil
    case DateFormCtxVar(formCtx)       => formCtx :: Nil
    case DateExprWithOffset(dExpr, _)  => allFormCtxExprs(dExpr)
    case HmrcTaxPeriodCtx(formCtx, _)  => formCtx :: Nil
    case DataRetrieveDateCtx(_, _)     => Nil
    case DateIfElse(_, field1, field2) => allFormCtxExprs(field1) ++ allFormCtxExprs(field2)
    case DateOrElse(field1, field2)    => allFormCtxExprs(field1) ++ allFormCtxExprs(field2)
    case DateConstructExpr(_, _)       => Nil
  }
}

object Expr2DateExpr {
  def unapply(expr: Expr): Option[DateExpr] = expr match {
    case DateCtx(dExpr) => Some(dExpr)
    case e @ FormCtx(_) => Some(DateFormCtxVar(e))
    case _              => None
  }
}
