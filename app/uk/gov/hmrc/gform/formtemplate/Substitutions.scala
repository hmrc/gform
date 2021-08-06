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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import play.api.libs.json.{ JsDefined, JsObject, JsString, JsUndefined, JsValue }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormTemplateRaw, TextExpression, ValueExpr }

case class Substitutions(expressions: Map[ExpressionId, Expr]) extends AnyVal

object Substitutions {
  val empty = Substitutions(Map.empty[ExpressionId, Expr])

  def from(templateRaw: FormTemplateRaw): Opt[Substitutions] = {

    def fromJsValue(jsValue: JsValue): Opt[Substitutions] = {
      val maybeSubstitutions: Opt[List[(ExpressionId, Expr)]] =
        jsValue match {
          case JsObject(fields) =>
            fields.toList.traverse { case (k, v) =>
              val expressionId = ExpressionId(k)
              val maybeExpr: Opt[Expr] = v match {
                case JsString(exprStr) =>
                  val parsed: Opt[ValueExpr] = ValueParser.validate("${" + exprStr + "}")

                  parsed match {
                    case Right(TextExpression(expr)) => Right(expr)
                    case Right(_)                    => Left(UnexpectedState("Wrong expression: " + v))
                    case Left(unexpectedState)       => Left(unexpectedState)
                  }

                case nonString =>
                  Left(
                    UnexpectedState(
                      "Wrong expression, expected JsString, but got " + nonString.getClass.getSimpleName + ": " + nonString
                    )
                  )
              }
              maybeExpr.map(expressionId -> _)
            }

          case nonJsObject =>
            Left(
              UnexpectedState(
                "Field 'expressions' needs to be a JsObject, but got " + nonJsObject.getClass.getSimpleName + ": " + nonJsObject
              )
            )
        }
      maybeSubstitutions.map(_.toMap).map(Substitutions.apply)
    }

    templateRaw.value \ "expressions" match {
      case JsDefined(json) => fromJsValue(json)
      case JsUndefined()   => Right(Substitutions.empty)
    }
  }
}

case class ExpressionId(id: String) extends AnyVal
