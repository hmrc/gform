/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.BasicParsers._
import uk.gov.hmrc.gform.core.parsers.ValueParser._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object BooleanExprParser {

  def validate(expression: String): Opt[BooleanExpr] = validateWithParser(expression, exprDeterminer)

  lazy val exprDeterminer: Parser[BooleanExpr] = booleanExpr ^^ { (loc, expr) =>
    expr
  }

  lazy val booleanExpr: Parser[BooleanExpr] = ("${" ~> basicExpressionParser <~ "}"
    | "${" ~ basicExpressionParser ~ booleanOperation ~ basicExpressionParser ~ "}" ^^ {
      (loc, _, expr1, op, expr2, _) =>
        Or(expr1, expr2)
    }
    | "True" ^^ { (loc, value) =>
      IsTrue
    }
    | booleanExpr ~ booleanOperation ~ booleanExpr ^^ { (loc, expr1, op, expr2) =>
      Or(expr1, expr2)
    })

  lazy val basicExpressionParser: Parser[BooleanExpr] =
    contextField ~ comparisonOperation ~ constant ^^ { (loc, expr1, op, expr2) =>
      Equals(expr1, expr2)
    }

  lazy val constant = ("'" ~ stringConstant ~ "'" ^^ { (loc, _, str, _) =>
    str
  }
    | "''" ^^ { (loc, str) =>
      Constant("")
    }
    | anyConstant)

  lazy val stringConstant: Parser[Constant] = """[ \w,]+""".r ^^ { (loc, str) =>
    Constant(str)
  }

  lazy val anyConstant: Parser[Constant] = """[ \w,]+""".r ^^ { (loc, str) =>
    Constant(str.trim)
  }

  lazy val comparisonOperation: Parser[Comparison] = "=" ^^ { (loc, _) =>
    Equality
  }

  lazy val booleanOperation: Parser[BooleanOperation] = "||" ^^ { (loc, _) =>
    OrOperation
  }

  lazy val operation: Parser[Operation] = ("+" ^^ { (loc, _) =>
    Addition
  }
    | "*" ^^ { (loc, _) =>
      Multiplication
    })
}
