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

package uk.gov.hmrc.gform.core.parsers

import parseback._
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

  implicit val W: Whitespace = Whitespace(() | """\s+""".r)

  private lazy val p0: Parser[BooleanExpr] = "true" ^^^ IsTrue |
    "yes" ^^^ IsTrue |
    "false" ^^^ IsFalse |
    "no" ^^^ IsFalse |
    "form.phase.is.instructionPDF" ^^ { (loc, _) =>
      FormPhase(InstructionPDF)
    } |
    FormComponentId.unanchoredIdValidation ^^ { (_, fcId) =>
      TopLevelRef(BooleanExprId(fcId))
    } |
    "(" ~> p4 <~ ")"

  lazy val quoteRegexParse: Parser[Regex] = "'" ~> "[^']+".r <~ "'" ^^ { (loc, regex) =>
    regex.r
  }

  private lazy val formCtxParse: Parser[FormCtx] = FormComponentId.unanchoredIdValidation ^^ { (_, fcId) =>
    FormCtx(FormComponentId(fcId))
  }

  private lazy val p1: Parser[BooleanExpr] = (exprFormCtx ~ "<" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
    LessThan(expr1, expr2)
  }
    | exprFormCtx ~ "<=" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
      LessThanOrEquals(expr1, expr2)
    }
    | exprFormCtx ~ "=" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
      Equals(expr1, expr2)
    }
    | exprFormCtx ~ "!=" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
      Not(Equals(expr1, expr2))
    }
    | exprFormCtx ~ ">=" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
      GreaterThanOrEquals(expr1, expr2)
    }
    | exprFormCtx ~ ">" ~ exprFormCtx ^^ { (loc, expr1, op, expr2) =>
      GreaterThan(expr1, expr2)
    }
    | dateExpr ~ "before" ~ dateExpr ^^ { (_, expr1, _, expr2) =>
      DateBefore(expr1, expr2)
    }
    | dateExpr ~ "after" ~ dateExpr ^^ { (_, expr1, _, expr2) =>
      DateAfter(expr1, expr2)
    }
    | formCtxParse ~ "contains" ~ exprFormCtx ^^ { (_, formCtx, _, expr) =>
      Contains(formCtx, expr)
    }
    | formCtxParse ~ "match" ~ quoteRegexParse ^^ { (_, formCtx, _, regex) =>
      MatchRegex(formCtx, regex)
    }
    | contextField ~ "in" ~ dataSourceParse ^^ { (_, expr, _, dataSource) =>
      In(expr, dataSource)
    }
    | p0)

  private lazy val p2: Parser[BooleanExpr] = ("!" ~ p1 ^^ { (loc, _, e) =>
    Not(e)
  }
    | p1)

  private lazy val p3: Parser[BooleanExpr] = (p3 ~ "&&" ~ p2 ^^ { (loc, expr1, op, expr2) =>
    And(expr1, expr2)
  }
    | p2)

  lazy val p4: Parser[BooleanExpr] = (p4 ~ "||" ~ p3 ^^ { (loc, expr1, op, expr2) =>
    Or(expr1, expr2)
  }
    | p3)

  lazy val booleanExpr: Parser[BooleanExpr] = "${" ~ p4 ~ "}" ^^ { (loc, _, e, _) =>
    e
  }

  def validate(expression: String): Opt[BooleanExpr] =
    validateWithParser(expression, booleanExpr)
}
