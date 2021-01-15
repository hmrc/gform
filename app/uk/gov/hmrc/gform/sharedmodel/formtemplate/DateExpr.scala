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

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait DateExpr

sealed trait OffsetUnit
case object OffsetUnitDay extends OffsetUnit
case object OffsetUnitYear extends OffsetUnit
case object OffsetUnitMonth extends OffsetUnit

object OffsetUnit {
  implicit val format: OFormat[OffsetUnit] = derived.oformat()
}

sealed trait DateExprValue
case object TodayDateExprValue extends DateExprValue
case class ExactDateExprValue(year: Int, month: Int, day: Int) extends DateExprValue

object DateExprValue {
  implicit val format: OFormat[DateExprValue] = derived.oformat()
}

case class DateValueExpr(value: DateExprValue) extends DateExpr
case class DateFormCtxVar(formCtx: FormCtx) extends DateExpr
case class DateExprWithOffset(dExpr: DateExpr, offset: Int, offsetUnit: OffsetUnit) extends DateExpr

object DateExpr {
  implicit val format: OFormat[DateExpr] = derived.oformat()

  def allFormCtxExprs(dateExpr: DateExpr): List[FormCtx] = dateExpr match {
    case DateValueExpr(_)                => Nil
    case DateFormCtxVar(formCtx)         => formCtx :: Nil
    case DateExprWithOffset(dExpr, _, _) => allFormCtxExprs(dExpr)
  }
}
