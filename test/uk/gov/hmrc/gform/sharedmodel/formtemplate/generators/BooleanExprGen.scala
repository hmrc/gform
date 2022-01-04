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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait BooleanExprGen {
  private def binaryExprRelationGen[T <: BooleanExpr](maxDepth: Int, f: (Expr, Expr) => T): Gen[T] =
    for {
      left  <- ExprGen.exprGen(maxDepth - 1)
      right <- ExprGen.exprGen(maxDepth - 1)
    } yield f(left, right)

  private def binaryBooleanExprRelationGen[T <: BooleanExpr](
    maxDepth: Int,
    f: (BooleanExpr, BooleanExpr) => T
  ): Gen[T] =
    for {
      left  <- booleanExprGen(maxDepth - 1)
      right <- booleanExprGen(maxDepth - 1)
    } yield f(left, right)

  def equalsGen(maxDepth: Int): Gen[Equals] = binaryExprRelationGen(maxDepth, Equals(_, _))
  def greaterThanGen(maxDepth: Int): Gen[GreaterThan] = binaryExprRelationGen(maxDepth, GreaterThan(_, _))
  def greaterThanOrEqualsGen(maxDepth: Int): Gen[GreaterThanOrEquals] =
    binaryExprRelationGen(maxDepth, GreaterThanOrEquals(_, _))
  def lessThanGen(maxDepth: Int): Gen[LessThan] = binaryExprRelationGen(maxDepth, LessThan(_, _))
  def lessThanOrEqualsGen(maxDepth: Int): Gen[LessThanOrEquals] =
    binaryExprRelationGen(maxDepth, LessThanOrEquals(_, _))

  def notGen(maxDepth: Int): Gen[Not] = booleanExprGen(maxDepth - 1).map(Not)
  def orGen(maxDepth: Int): Gen[Or] = binaryBooleanExprRelationGen(maxDepth, Or(_, _))
  def andGen(maxDepth: Int): Gen[And] = binaryBooleanExprRelationGen(maxDepth, And(_, _))

  def nonRecursiveBooleanExprGen: Gen[BooleanExpr] = Gen.oneOf(Gen.const(IsTrue), Gen.const(IsFalse))

  def recursiveBooleanExprGen(maxDepth: Int = 3): Gen[BooleanExpr] = Gen.oneOf(
    equalsGen(maxDepth),
    greaterThanGen(maxDepth),
    greaterThanOrEqualsGen(maxDepth),
    lessThanGen(maxDepth),
    lessThanOrEqualsGen(maxDepth),
    notGen(maxDepth),
    orGen(maxDepth),
    andGen(maxDepth)
  )

  def booleanExprGen(maxDepth: Int = 3): Gen[BooleanExpr] =
    if (maxDepth <= 1) nonRecursiveBooleanExprGen
    else Gen.oneOf(nonRecursiveBooleanExprGen, recursiveBooleanExprGen(maxDepth))
}

object BooleanExprGen extends BooleanExprGen
