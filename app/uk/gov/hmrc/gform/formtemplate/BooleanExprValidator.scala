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

import cats.implicits._
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.ValidationResult.BooleanToValidationResultSyntax
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprValidator(
  indexLookup: Map[FormComponentId, Int],
  wrapperType: BooleanExprWrapperType,
  addToListIds: List[FormComponentId]
)(
  pageIdx: Int
) {

  private def extractFcIds(expr: Expr): List[FormComponentId] = expr match {
    case Add(left, right)         => extractFcIds(left) ::: extractFcIds(right)
    case Subtraction(left, right) => extractFcIds(left) ::: extractFcIds(right)
    case Multiply(left, right)    => extractFcIds(left) ::: extractFcIds(right)
    case Divide(left, right)      => extractFcIds(left) ::: extractFcIds(right)
    case Sum(field1)              => extractFcIds(field1)
    case FormCtx(fcId)            => List(fcId)
    case _                        => Nil
  }

  private def extractFcIds(expr: DateExpr): List[FormComponentId] = expr match {
    case DateExprWithOffset(dateExpr, _)          => extractFcIds(dateExpr)
    case DateFormCtxVar(FormCtx(formComponentId)) => List(formComponentId)
    case HmrcTaxPeriodCtx(FormCtx(fcId), _)       => List(fcId)
    case DateIfElse(_, field1, field2)            => extractFcIds(field1) ++ extractFcIds(field2)
    case _                                        => Nil
  }

  private def validateIdIdx(id: FormComponentId) =
    indexLookup
      .get(id)
      .map { formComponentPageIdx =>
        wrapperType
          .isForwardReference(formComponentPageIdx, pageIdx)
          .validationResult(s"id '${id.value}' named in $wrapperType is forward reference, which is not permitted")
      }
      .getOrElse(Invalid(s"id '${id.value}' named in $wrapperType expression does not exist in the form"))

  private def validateExprs(left: Expr, right: Expr): List[ValidationResult] =
    validateExprs(List(left, right))

  private def validateExprs(xs: List[Expr]): List[ValidationResult] =
    xs
      .flatMap(extractFcIds)
      .map(validateIdIdx)

  private def validateDateExprs(left: DateExpr, right: DateExpr): List[ValidationResult] =
    List(left, right)
      .flatMap(extractFcIds)
      .map(validateIdIdx)

  private def validateValueField(expr: Expr): List[ValidationResult] =
    extractFcIds(expr).map(validateIdIdx)

  private def validateAddToListId(expr: Expr): List[ValidationResult] =
    extractFcIds(expr).map { fcId =>
      addToListIds
        .find(_ === fcId)
        .map { _ =>
          Valid
        }
        .getOrElse(Invalid(s"page id '${fcId.value}' does not exist in the form"))
    }

  private def validateIdList(fields: List[FormCtx]): List[ValidationResult] =
    fields
      .flatMap(extractFcIds)
      .map(validateIdIdx)

  def apply(includeIf: BooleanExpr): List[ValidationResult] = includeIf match {
    case Equals(left, right)                          => validateExprs(left, right)
    case GreaterThan(left, right)                     => validateExprs(left, right)
    case GreaterThanOrEquals(left, right)             => validateExprs(left, right)
    case LessThan(left, right)                        => validateExprs(left, right)
    case LessThanOrEquals(left, right)                => validateExprs(left, right)
    case Not(e)                                       => apply(e)
    case Or(left, right)                              => apply(left) ::: apply(right)
    case And(left, right)                             => apply(left) ::: apply(right)
    case IsFalse | IsTrue | FormPhase(_) | IsLogin(_) => List(Valid)
    case Contains(collection, value)                  => validateExprs(collection, value)
    case In(value, _)                                 => validateValueField(value)
    case HasAnswer(formCtx, addToListRef)             => validateExprs(formCtx :: addToListRef.allExpressions.toList)
    case DateBefore(left, right)                      => validateDateExprs(left, right)
    case DateAfter(left, right)                       => validateDateExprs(left, right)
    case MatchRegex(value, _)                         => validateValueField(value)
    case TopLevelRef(_)                               => Nil
    case First(value)                                 => validateAddToListId(value)
    case DuplicateExists(fieldList)                   => validateIdList(fieldList)
  }
}

sealed trait BooleanExprWrapperType {
  import BooleanExprWrapperType._
  def isForwardReference(idx1: Int, idx2: Int): Boolean = this match {
    case ValidIf      => idx1 <= idx2
    case IncludeIf    => idx1 < idx2
    case RemoveItemIf => idx1 <= idx2
  }

  override def toString: String = this match {
    case ValidIf      => "validIf"
    case IncludeIf    => "includeIf"
    case RemoveItemIf => "removeItemIf"
  }
}
object BooleanExprWrapperType {
  case object ValidIf extends BooleanExprWrapperType
  case object IncludeIf extends BooleanExprWrapperType
  case object RemoveItemIf extends BooleanExprWrapperType
}
