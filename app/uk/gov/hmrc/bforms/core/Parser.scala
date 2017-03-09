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
import parseback._
import parseback.compat.cats._
import parseback.util.Catenable

sealed trait Expr

final case class Add(field1: Expr, field2: Expr) extends Expr
final case class Multiply(field1: Expr, field2: Expr) extends Expr
final case class FormCtx(value: String) extends Expr
final case class AuthCtx(value: String) extends Expr
final case class EeittCtx(value: String) extends Expr
final case class Constant(value: String) extends Expr

sealed trait Operation
final case object Addition extends Operation
final case object Multiplication extends Operation

sealed trait Context
final case object FormContext extends Context
final case object AuthContext extends Context
final case object EeittContext extends Context

object Parser {

  def apply(s: String): Either[List[ParseError], Catenable[Expr]] = expr(LineStream[Eval](s)).value

  implicit val W = Whitespace(() | """\s+""".r)

  lazy val expr: Parser[Expr] = (
    "${" ~ contextField ~ "}" ^^ { (loc, _, field, _) => field }
    | "${" ~ contextField ~ operation ~ contextField ~ "}" ^^ { (loc, _, field1, op, field2, _) =>
      op match {
        case Addition => Add(field1, field2)
        case Multiplication => Multiply(field1, field2)
      }
    }
    | alphabeticOnly ^^ { (loc, const) => Constant(const) }
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

  lazy val alphabeticOnly: Parser[String] = (
    """\w+""".r ^^ { (loc, str) => str }
  )

  lazy val context: Parser[Context] = (
    "form" ^^ { (loc, str) => FormContext }
    | "auth" ^^ { (loc, str) => AuthContext }
    | "eeitt" ^^ { (loc, str) => EeittContext }
  )

}
