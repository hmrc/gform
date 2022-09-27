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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExprSubstituter extends Substituter[ExprSubstitutions, FormTemplate] {

  import uk.gov.hmrc.gform.formtemplate.Substituter.SubstituterSyntax

  implicit val exprSubstituter: Substituter[ExprSubstitutions, Expr] =
    new Substituter[ExprSubstitutions, Expr] {
      def substitute(substitutions: ExprSubstitutions, t: Expr): Expr = t match {
        case Else(l, r)             => Else(substitute(substitutions, l), substitute(substitutions, r))
        case Add(l, r)              => Add(substitute(substitutions, l), substitute(substitutions, r))
        case Multiply(l, r)         => Multiply(substitute(substitutions, l), substitute(substitutions, r))
        case Subtraction(l, r)      => Subtraction(substitute(substitutions, l), substitute(substitutions, r))
        case Divide(l, r)           => Divide(substitute(substitutions, l), substitute(substitutions, r))
        case Period(l, r)           => Period(substitute(substitutions, l), substitute(substitutions, r))
        case Sum(l)                 => Sum(substitute(substitutions, l))
        case PeriodExt(p, pe)       => PeriodExt(substitute(substitutions, p), pe)
        case DateCtx(dateExpr)      => DateCtx(dateExpr(substitutions))
        case DateFunction(dateFunc) => DateFunction(dateFunc(substitutions))
        case i @ IfElse(cond, l, r) =>
          IfElse(cond(substitutions), substitute(substitutions, l), substitute(substitutions, r))
        case f @ FormCtx(formComponentId) =>
          // Replace FormComponentId with top level expression if one exists
          substitutions.expressions.getOrElse(ExpressionId(formComponentId.value), f)
        case AddressLens(_, _)             => t
        case AuthCtx(_)                    => t
        case Constant(_)                   => t
        case Count(_)                      => t
        case FormTemplateCtx(_)            => t
        case HmrcRosmRegistrationCheck(_)  => t
        case LangCtx                       => t
        case LinkCtx(_)                    => t
        case ParamCtx(_)                   => t
        case PeriodValue(_)                => t
        case UserCtx(_)                    => t
        case Value                         => t
        case DataRetrieveCtx(_, _)         => t
        case CsvCountryCheck(_, _)         => t
        case CsvOverseasCountryCheck(_, _) => t
        case CsvCountryCountCheck(_, _, _) => t
        case Size(_, _)                    => t
        case Typed(expr, tpe)              => Typed(substitute(substitutions, expr), tpe)
      }
    }

  implicit val dateProjectionSubstituter: Substituter[ExprSubstitutions, DateProjection] = (substitutions, t) =>
    t match {
      case d @ DateProjection.Day(dateExpr)   => DateProjection.Day(dateExpr(substitutions))
      case d @ DateProjection.Month(dateExpr) => DateProjection.Month(dateExpr(substitutions))
      case d @ DateProjection.Year(dateExpr)  => DateProjection.Year(dateExpr(substitutions))
    }

  implicit val dateExprSubstituter: Substituter[ExprSubstitutions, DateExpr] = (substitutions, t) => {
    def aux(dExpr: DateExpr): DateExpr =
      dExpr match {
        case d @ DateFormCtxVar(FormCtx(formComponentId)) =>
          substitutions.expressions.get(ExpressionId(formComponentId.value)) match {
            case Some(DateCtx(dateExpr))                   => dateExpr
            case Some(ctx @ FormCtx(_))                    => DateFormCtxVar(ctx)
            case Some(IfElse(c, DateCtx(f1), DateCtx(f2))) => DateIfElse(c, aux(f1), aux(f2))
            case here                                      => d
          }
        case d @ HmrcTaxPeriodCtx(FormCtx(fcId), _) =>
          substitutions.expressions.get(ExpressionId(fcId.value)) match {
            case Some(DateCtx(dateExpr))                   => dateExpr
            case Some(ctx @ FormCtx(_))                    => DateFormCtxVar(ctx)
            case Some(IfElse(c, DateCtx(f1), DateCtx(f2))) => DateIfElse(c, aux(f1), aux(f2))
            case here                                      => d
          }
        case d @ DateValueExpr(_) => d
        case DateExprWithOffset(dExpr, offset) =>
          dExpr(substitutions) match {
            case DateExprWithOffset(expr, innerOffset) => DateExprWithOffset(expr, innerOffset + offset)
            case other                                 => DateExprWithOffset(other, offset)
          }
        case DateIfElse(cond, field1, field2) => DateIfElse(cond, aux(field1), aux(field2))
        case DateOrElse(field1, field2)       => DateOrElse(aux(field1), aux(field2))
      }
    aux(t)
  }

  implicit val booleanExprSubstituter: Substituter[ExprSubstitutions, BooleanExpr] =
    new Substituter[ExprSubstitutions, BooleanExpr] {
      def substitute(substitutions: ExprSubstitutions, t: BooleanExpr): BooleanExpr = t match {
        case Equals(l, r)                     => Equals(l(substitutions), r(substitutions))
        case GreaterThan(l, r)                => GreaterThan(l(substitutions), r(substitutions))
        case GreaterThanOrEquals(l, r)        => GreaterThanOrEquals(l(substitutions), r(substitutions))
        case LessThan(l, r)                   => LessThan(l(substitutions), r(substitutions))
        case LessThanOrEquals(l, r)           => LessThanOrEquals(l(substitutions), r(substitutions))
        case Not(e)                           => Not(substitute(substitutions, e))
        case Or(left, right)                  => Or(substitute(substitutions, left), substitute(substitutions, right))
        case And(left, right)                 => And(substitute(substitutions, left), substitute(substitutions, right))
        case IsTrue                           => IsTrue
        case IsFalse                          => IsFalse
        case Contains(multiValueField, value) => Contains(multiValueField, value(substitutions))
        case In(value, dataSource)            => In(value(substitutions), dataSource)
        case m @ MatchRegex(expr, regex)      => m
        case DateBefore(l, r)                 => DateBefore(l(substitutions), r(substitutions))
        case DateAfter(l, r)                  => DateAfter(l(substitutions), r(substitutions))
        case f @ FormPhase(value)             => f
        case t @ TopLevelRef(id)              => t
        case f @ First(_)                     => f
        case l @ IsLogin(_)                   => l
      }
    }

  def substitute(substitutions: ExprSubstitutions, t: FormTemplate): FormTemplate =
    implicitly[Substituter[ExprSubstitutions, FormTemplate]].substitute(substitutions, t)

  def substituteExpr(substitutions: ExprSubstitutions, t: Expr): Expr =
    implicitly[Substituter[ExprSubstitutions, Expr]].substitute(substitutions, t)
}

trait SubstituteExpressions {

  def substituteExpressions(formTemplate: FormTemplate, substitutions: ExprSubstitutions): FormTemplate =
    ExprSubstituter.substitute(substitutions, formTemplate)

}
