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

import cats.instances.string._
import cats.syntax.eq._
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.constraints.{ ReferenceInfo }

case class TemplatePath(path: String) extends AnyVal {
  def +(subPath: String) = if (path === TemplatePath.root.path)
    TemplatePath(subPath)
  else
    TemplatePath(path + "." + subPath)

  def subpaths: List[String] = path.split('.').toList
}

object TemplatePath {
  val root = TemplatePath("/")
  val leaf = TemplatePath("")
}

final case class ExprWithPath(path: TemplatePath, expr: Expr) {

  private def toReferenceInfo(es: Expr*): List[ReferenceInfo] =
    es.toList.flatMap(e => ExprWithPath(path, e).referenceInfos)

  private def booleanExprRefInfo(booleanExpr: BooleanExpr): List[ReferenceInfo] =
    implicitly[LeafExpr[BooleanExpr]].exprs(path, booleanExpr).flatMap(_.referenceInfos)

  def referenceInfos: List[ReferenceInfo] = expr match {
    case Add(field1: Expr, field2: Expr)               => toReferenceInfo(field1, field2)
    case Multiply(field1: Expr, field2: Expr)          => toReferenceInfo(field1, field2)
    case Subtraction(field1: Expr, field2: Expr)       => toReferenceInfo(field1, field2)
    case Divide(field1: Expr, field2: Expr)            => toReferenceInfo(field1, field2)
    case IfElse(cond, field1: Expr, field2: Expr)      => booleanExprRefInfo(cond) ++ toReferenceInfo(field1, field2)
    case Else(field1: Expr, field2: Expr)              => toReferenceInfo(field1, field2)
    case f @ FormCtx(formComponentId: FormComponentId) => ReferenceInfo.FormCtxExpr(path, f) :: Nil
    case s @ Sum(field1: Expr)                         => ReferenceInfo.SumExpr(path, s) :: Nil
    case c @ Count(formComponentId: FormComponentId)   => ReferenceInfo.CountExpr(path, c) :: Nil
    case a @ AddressLens(_, _)                         => ReferenceInfo.AddressExpr(path, a) :: Nil
    case AuthCtx(value: AuthInfo)                      => Nil
    case UserCtx(value: UserField)                     => Nil
    case Constant(value: String)                       => Nil
    case PeriodValue(value: String)                    => Nil
    case HmrcRosmRegistrationCheck(value: RosmProp)    => Nil
    case Value                                         => Nil
    case LangCtx                                       => Nil
    case FormTemplateCtx(value: FormTemplateProp)      => Nil
    case ParamCtx(_)                                   => Nil
    case l @ LinkCtx(_)                                => ReferenceInfo.LinkCtxExpr(path, l) :: Nil
    case DateCtx(dateExpr)                             => dateExpr.referenceInfos
    case DateFunction(value)                           => ReferenceInfo.DateFunctionExpr(path, value) :: Nil
    case p @ Period(_, _)                              => ReferenceInfo.PeriodExpr(path, p) :: Nil
    case p @ PeriodExt(_, _)                           => ReferenceInfo.PeriodExtExpr(path, p) :: Nil
    case d @ DataRetrieveCtx(_, _)                     => ReferenceInfo.DataRetrieveCtxExpr(path, d) :: Nil
    case c @ CsvCountryCheck(_, _)                     => ReferenceInfo.CsvCountryCheckExpr(path, c) :: Nil
    case c @ CsvOverseasCountryCheck(_, _)             => ReferenceInfo.CsvOverseasCountryCheckExpr(path, c) :: Nil
    case c @ CsvCountryCountCheck(_, _, _)             => ReferenceInfo.CsvCountryCountCheckExpr(path, c) :: Nil
    case s @ Size(_, _)                                => ReferenceInfo.SizeExpr(path, s) :: Nil
    case Typed(expr, tpe)                              => toReferenceInfo(expr)
    case s @ IndexOf(_, _)                             => ReferenceInfo.IndexOfExpr(path, s) :: Nil
    case r @ RemoveSpaces(_)                           => ReferenceInfo.RemoveSpacesExpr(path, r) :: Nil
  }
}

abstract class LeafExpr[-T] {
  def exprs(path: TemplatePath, t: T): List[ExprWithPath]
}

object LeafExpr {

  def apply[T](path: TemplatePath, t: T)(implicit le: LeafExpr[T]): List[ExprWithPath] = le.exprs(path, t)

  implicit def optionalLeafExpr[T: LeafExpr] = new LeafExpr[Option[T]] {
    def exprs(path: TemplatePath, t: Option[T]): List[ExprWithPath] = t match {
      case None    => Nil
      case Some(x) => implicitly[LeafExpr[T]].exprs(path, x)
    }
  }

  implicit def listLeafExpr[T: LeafExpr] = new LeafExpr[List[T]] {
    def exprs(path: TemplatePath, ts: List[T]): List[ExprWithPath] = {
      val leafExprT = implicitly[LeafExpr[T]]
      ts.flatMap(t => leafExprT.exprs(path, t))
    }
  }

  implicit def nelLeafExpr[T: LeafExpr] = new LeafExpr[NonEmptyList[T]] {
    def exprs(path: TemplatePath, ts: NonEmptyList[T]): List[ExprWithPath] =
      implicitly[LeafExpr[List[T]]].exprs(path, ts.toList)
  }
}
