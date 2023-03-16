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

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object BooleanExprSubstituter extends Substituter[BooleanExprSubstitutions, FormTemplate] {

  import uk.gov.hmrc.gform.formtemplate.Substituter.SubstituterSyntax

  implicit val exprSubstituter: Substituter[BooleanExprSubstitutions, Expr] =
    new Substituter[BooleanExprSubstitutions, Expr] {
      def substitute(substitutions: BooleanExprSubstitutions, t: Expr): Expr = t match {
        case Else(l, r)            => Else(substitute(substitutions, l), substitute(substitutions, r))
        case Add(l, r)             => Add(substitute(substitutions, l), substitute(substitutions, r))
        case Multiply(l, r)        => Multiply(substitute(substitutions, l), substitute(substitutions, r))
        case Subtraction(l, r)     => Subtraction(substitute(substitutions, l), substitute(substitutions, r))
        case Divide(l, r)          => Divide(substitute(substitutions, l), substitute(substitutions, r))
        case Period(l, r)          => Period(substitute(substitutions, l), substitute(substitutions, r))
        case Sum(l)                => Sum(substitute(substitutions, l))
        case PeriodExt(p, pe)      => PeriodExt(substitute(substitutions, p), pe)
        case d @ DateCtx(dateExpr) => d
        case d @ DateFunction(_)   => d
        case i @ IfElse(cond, l, r) =>
          IfElse(cond(substitutions), substitute(substitutions, l), substitute(substitutions, r))
        case f @ FormCtx(formComponentId)  => f
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
        case DataRetrieveCount(_)          => t
        case CsvCountryCheck(_, _)         => t
        case CsvOverseasCountryCheck(_, _) => t
        case CsvCountryCountCheck(_, _, _) => t
        case Size(_, _)                    => t
        case Typed(expr, tpe)              => Typed(substitute(substitutions, expr), tpe)
        case IndexOf(_, _)                 => t
        case IndexOfDataRetrieveCtx(_, _)  => t
        case NumberedList(_)               => t
        case BulletedList(_)               => t
        case StringOps(_, _)               => t
        case Concat(exprs)                 => Concat(exprs.map(substitute(substitutions, _)))
      }
    }

  implicit val booleanExprSubstituter: Substituter[BooleanExprSubstitutions, BooleanExpr] =
    new Substituter[BooleanExprSubstitutions, BooleanExpr] {
      def substitute(substitutions: BooleanExprSubstitutions, t: BooleanExpr): BooleanExpr = t match {
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
        case d @ DateBefore(l, r)             => d
        case d @ DateAfter(l, r)              => d
        case f @ FormPhase(value)             => f
        case TopLevelRef(id) =>
          substitute(
            substitutions,
            substitutions.expressions
              .getOrElse(id, throw new NoSuchElementException(s"$id not found in booleanExpressions"))
          )
        case f @ First(_)   => f
        case l @ IsLogin(_) => l
      }
    }

  def substitute(substitutions: BooleanExprSubstitutions, t: FormTemplate): FormTemplate =
    implicitly[Substituter[BooleanExprSubstitutions, FormTemplate]].substitute(substitutions, t)
}

trait SubstituteBooleanExprs {
  import uk.gov.hmrc.gform.formtemplate.Substituter.SubstituterSyntax
  import uk.gov.hmrc.gform.formtemplate.ExprSubstituter.booleanExprSubstituter

  def substituteBooleanExprs(
    formTemplate: FormTemplate,
    substitutions: BooleanExprSubstitutions,
    exprSubstitutions: ExprSubstitutions
  ): FormTemplate = {
    val substitutedBooleanExpressions = substitutions.expressions.map { case (id, boolExpr) =>
      (id, boolExpr(exprSubstitutions))
    }.toMap
    BooleanExprSubstituter.substitute(BooleanExprSubstitutions(substitutedBooleanExpressions), formTemplate)
  }

}
