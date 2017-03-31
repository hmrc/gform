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

package uk.gov.hmrc.bforms.core

import cats.Eval
import cats.data.ReaderT
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable
import uk.gov.hmrc.bforms.exceptions.InvalidState

object Parser {

  private def parse = ReaderT[Opt, String, Catenable[Expr]] { expression =>
    expr(LineStream[Eval](expression)).value.leftMap { error =>
      val errors: String = error.map(_.render(expression)).mkString("\n")
      InvalidState(s"""|Unable to parse expression $expression.
                       |Errors:
                       |$errors""".stripMargin)
    }
  }

  private def reconstruct(cat: Catenable[Expr]) = ReaderT[Opt, String, Expr] { expression =>
    cat.uncons match {
      case Some((expr, _)) => Right(expr)
      case None => Left(InvalidState(s"Unable to parse expression $expression"))
    }
  }

  def validate(expression: String): Opt[Expr] = (for {
    catenable <- parse
    expr <- reconstruct(catenable)
  } yield expr).run(expression)

  def validateList(expressions: List[String]): Opt[List[Expr]] =
    expressions.map(validate).sequence

  implicit val W = Whitespace(() | """\s+""".r)

  lazy val expr: Parser[Expr] = (
    "${" ~ contextField ~ "}" ^^ { (loc, _, field, _) => field }
    | "${" ~ contextField ~ operation ~ contextField ~ "}" ^^ { (loc, _, field1, op, field2, _) =>
      op match {
        case Addition => Add(field1, field2)
        case Multiplication => Multiply(field1, field2)
      }
    }
    | dateOnly ^^ { (loc, dateExpr) => dateExpr }
    | anyConstant ^^ { (loc, const) => const }
  )

  lazy val operation: Parser[Operation] = (
    "+" ^^ { (loc, _) => Addition }
    | "*" ^^ { (loc, _) => Multiplication }
  )

  lazy val contextField: Parser[Expr] = (
    context ~ "." ~ alphabeticOnly ^^ { (loc, ctx, _, fieldName) =>
      ctx match {
        case FormContext => FormCtx(fieldName)
        case AuthContext => AuthCtx(fieldName)
        case EeittContext => EeittCtx(fieldName)
      }
    }
    | alphabeticOnly ^^ { (loc, fn) => FormCtx(fn) }
  )

  lazy val dateOnly: Parser[DateExpr] = {
    """^(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d$""".r ^^ { (loc, str) =>

      val values = str.split("-")
      DateExpr(values(0), values(1), values(2))
    }
  }

  lazy val alphabeticOnly: Parser[String] = (
    """\w+""".r ^^ { (loc, str) => str }
  )

  lazy val anyConstant: Parser[Constant] = (
    """[\w,]+""".r ^^ { (loc, str) => Constant(str) }
  )

  lazy val context: Parser[Context] = (
    "form" ^^ { (loc, str) => FormContext }
    | "auth" ^^ { (loc, str) => AuthContext }
    | "eeitt" ^^ { (loc, str) => EeittContext }
  )

}
