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

import parseback.{ Parser, _ }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers.{ validateWithParser, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExprParsers {

  def validateFormCtx(expression: String): Opt[FormCtx] = validateWithParser(expression, expr)

  lazy val expr: Parser[FormCtx] = "$" ~ contextField ~ "" ^^ { (loc, _, field, _) => field }

  lazy val contextField: Parser[FormCtx] = alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) }

  lazy val alphabeticOnly: Parser[String] = """\w+""".r ^^ { (loc, str) => str }

  def validateExpr(expression: String): Opt[Expr] = validateWithParser(expression, exprFormCtx)

  lazy val exprFormCtx: Parser[Expr] = (
    "'" ~ anyConstant ~ "'" ^^ { (loc, _, str, _) => str }
    | parserExpression)

  lazy val contextFieldExpr: Parser[Expr] = (
    "${" ~> parserExpression <~ "}"
    | "eeitt" ~ "." ~ eeitt ^^ { (loc, _, _, eeitt) => EeittCtx(eeitt) }
    | "user" ~ "." ~ userField ^^ { (loc, _, _, userField) => UserCtx(userField) }
    | "form" ~ "." ~ alphabeticOnly ^^ { (loc, _, _, fieldName) => FormCtx(fieldName) }
    | "auth" ~ "." ~ authInfo ^^ { (loc, _, _, authInfo) => AuthCtx(authInfo) }
    | alphabeticOnly ~ ".sum" ^^ { (loc, value, _) => Sum(FormCtx(value)) }
    | anyDigitConst ^^ { (loc, str) => str }
    | alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) })

  lazy val parserExpression: Parser[Expr] = (
    parserExpression ~ "+" ~ parserExpression ^^ { (loc, expr1, _, expr2) => Add(expr1, expr2) }
    | parserExpression ~ "*" ~ parserExpression ^^ { (loc, expr1, _, expr2) => Multiply(expr1, expr2) }
    | contextField)

  lazy val anyDigitConst: Parser[Expr] = (
    """[ \d,]+""".r ^^ { (loc, str) => Constant(str) })

  lazy val anyConstant: Parser[Constant] = (
    """[ \w,]+""".r ^^ { (loc, str) => Constant(str) })

  lazy val eeitt: Parser[Eeitt] = (
    "businessUser" ^^ { (loc, _) => BusinessUser }
    | "agent" ^^ { (loc, _) => Agent }
    | "userId" ^^ { (loc, _) => UserId })

  lazy val userField: Parser[UserField] =
    "affinityGroup" ^^ { (loc, _) => AffinityGroup }

  lazy val authInfo: Parser[AuthInfo] = (
    "gg" ^^ { (_, _) => GG }
    | "payenino" ^^ { (_, _) => PayeNino }
    | "sautr" ^^ { (_, _) => SaUtr }
    | "ctutr" ^^ { (_, _) => CtUtr })
}
