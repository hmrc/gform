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

package uk.gov.hmrc.gform.core.parsers

import cats.parse.Rfc5234.{ alpha, digit, sp }
import cats.parse.Parser
import cats.parse.Parser.{ char, string }

import scala.util.matching.Regex
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.core.parsers.ValueParser._
import uk.gov.hmrc.gform.formtemplate.BooleanExprId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object BooleanExprParser {

  // Operator precedence, increasing
  //
  // ||
  // &&
  // !
  // < <= = != >= > includes
  // ?

//  implicit val W: Whitespace = Whitespace(() | """\s+""".r)

  def token(token: String) = string(token).surroundedBy(sp.rep0)

  private lazy val p0: Parser[BooleanExpr] = token("true").map(_ => IsTrue) |
    token("yes").map(_ => IsTrue) |
    token("false").map(_ => IsFalse) |
    token("no").map(_ => IsFalse) |
    token("form.phase.is.instructionPDF").map { _ =>
      FormPhase(InstructionPDF)
    } |
    FormComponentId.unanchoredIdValidationParser.map { fcId =>
      TopLevelRef(BooleanExprId(fcId))
    } |
    token("(") *> p4 <* token(")")

  lazy val quoteRegexParse: Parser[Regex] =
    (char('\'') *> Parser.charsWhile(x => x != '\'') <* char('\'')).map(x => x.r)

  private lazy val formCtxParse: Parser[FormCtx] = FormComponentId.unanchoredIdValidationParser.map { fcId =>
    FormCtx(FormComponentId(fcId))
  }

  private val exprFormCtx: Parser[Expr] = ???

  private def comparisonParser(_token: String, f: (Expr, Expr) => BooleanExpr) =
    ((exprFormCtx <* token(_token)) ~ exprFormCtx).map { case (expl, expr) =>
      f(expl, expr)
    }

  private lazy val p1: Parser[BooleanExpr] =
    comparisonParser("<", LessThan) |
      comparisonParser("<=", LessThanOrEquals) |
      comparisonParser("=", Equals) |
      comparisonParser("!=", (x, y) => Not(Equals(x, y))) |
      comparisonParser(">=", GreaterThanOrEquals) |
      comparisonParser(">", GreaterThan) |
      ((dateExpr <* token("before")) ~ dateExpr).map { case (expr1, expr2) => DateBefore(expr1, expr2) } |
      ((dateExpr <* token("after")) ~ dateExpr).map { case (expr1, expr2) => DateAfter(expr1, expr2) } |
      ((formCtxParse <* token("contains")) ~ exprFormCtx).map { case (expr1, expr2) => Contains(expr1, expr2) } |
      ((formCtxParse <* token("match")) ~ quoteRegexParse).map { case (expr1, expr2) => MatchRegex(expr1, expr2) } |
      ((contextField <* token("in")) ~ dataSourceParse).map { case (expr1, expr2) => In(expr1, expr2) } | p0

  private lazy val p2: Parser[BooleanExpr] = (char('!') *> p1).map(Not) | p1

  private lazy val p3: Parser[BooleanExpr] = ((p3 <* token("&&")) ~ p2).map { case (expr1, expr2) =>
    And(expr1, expr2)
  } | p2

  lazy val p4: Parser[BooleanExpr] = ((p4 <* token("||")) ~ p3).map { case (expr1, expr2) => Or(expr1, expr2) } | p3

  lazy val booleanExpr: Parser[BooleanExpr] = token("${") *> p4 <* token("}")

  def validate(expression: String): Opt[BooleanExpr] =
    validateWithParser(expression, booleanExpr)
}
