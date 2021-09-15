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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait ExprGen {
  def authInfoGen: Gen[AuthInfo] = Gen.oneOf(GG, PayeNino, SaUtr, CtUtr)

  def serviceNameGen: Gen[ServiceName] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(ServiceName(_))

  def identifierNameGen: Gen[IdentifierName] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(IdentifierName(_))

  val userFieldFuncIndexGen: Gen[UserFieldFunc.Index] = for {
    index <- Gen.posNum[Int]
  } yield UserFieldFunc.Index(index)

  val userFieldFuncGen: Gen[UserFieldFunc] = Gen.oneOf[UserFieldFunc](UserFieldFunc.Count, userFieldFuncIndexGen)

  def enrolmentGen: Gen[UserField.Enrolment] =
    for {
      serviceName        <- serviceNameGen
      identifierName     <- identifierNameGen
      maybeUserFieldFunc <- Gen.option(userFieldFuncGen)
    } yield UserField.Enrolment(serviceName, identifierName, maybeUserFieldFunc)

  def userFieldGen: Gen[UserField] = Gen.oneOf(Gen.const(UserField.AffinityGroup), enrolmentGen)

  def addGen(maxDepth: Int): Gen[Add] =
    for {
      f1 <- exprGen(maxDepth - 1)
      f2 <- exprGen(maxDepth - 1)
    } yield Add(f1, f2)

  def multiplyGen(maxDepth: Int): Gen[Multiply] =
    for {
      f1 <- exprGen(maxDepth - 1)
      f2 <- exprGen(maxDepth - 1)
    } yield Multiply(f1, f2)

  def subtractionGen(maxDepth: Int): Gen[Subtraction] =
    for {
      f1 <- exprGen(maxDepth - 1)
      f2 <- exprGen(maxDepth - 1)
    } yield Subtraction(f1, f2)

  def sumGen(maxDepth: Int): Gen[Sum] =
    exprGen(maxDepth - 1).map(Sum)

  def formCtxGen: Gen[FormCtx] = FormComponentGen.formComponentIdGen.map(FormCtx(_))

  def authCtxGen: Gen[AuthCtx] = authInfoGen.map(AuthCtx)

  def userCtxGen: Gen[UserCtx] = userFieldGen.map(UserCtx)

  def constantGen: Gen[Constant] = Gen.alphaNumStr.map(Constant)

  def formTemplateCtxGen: Gen[FormTemplateCtx] =
    Gen.oneOf(FormTemplateCtx(FormTemplateProp.SubmissionReference), FormTemplateCtx(FormTemplateProp.Id))

  def nonRecursiveExprGen: Gen[Expr] =
    Gen.oneOf(formCtxGen, authCtxGen, userCtxGen, constantGen, Gen.const(Value), formTemplateCtxGen)

  def recursiveExprGen(maxDepth: Int = 3): Gen[Expr] =
    Gen.oneOf(addGen(maxDepth), multiplyGen(maxDepth), subtractionGen(maxDepth), sumGen(maxDepth))

  def exprGen(maxDepth: Int = 3): Gen[Expr] =
    if (maxDepth <= 1) nonRecursiveExprGen
    else Gen.oneOf(nonRecursiveExprGen, recursiveExprGen(maxDepth))
}

object ExprGen extends ExprGen
