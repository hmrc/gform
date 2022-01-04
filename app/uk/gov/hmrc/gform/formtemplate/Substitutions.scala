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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import julienrf.json.derived
import play.api.libs.json.{ JsDefined, JsObject, JsString, JsUndefined, JsValue, OFormat }
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.BooleanExpr
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormTemplateRaw, TextExpression, ValueExpr }

sealed trait Substitutions[K, V] {

  def fieldName: String

  def fromJsValue(jsValue: JsValue)(toKey: String => K)(f: String => Opt[V]): Opt[Map[K, V]] = {
    val maybeSubstitutions: Opt[List[(K, V)]] =
      jsValue match {
        case JsObject(fields) =>
          fields.toList.traverse { case (k, v) =>
            val maybeExpr: Opt[V] = v match {
              case JsString(exprStr) => f(exprStr)
              case nonString =>
                Left(
                  UnexpectedState(
                    s"Wrong '$fieldName', expected JsString, but got " + nonString.getClass.getSimpleName + ": " + nonString
                  )
                )
            }
            maybeExpr.map(toKey(k) -> _)
          }

        case nonJsObject =>
          Left(
            UnexpectedState(
              s"Field '$fieldName' needs to be a JsObject, but got " + nonJsObject.getClass.getSimpleName + ": " + nonJsObject
            )
          )
      }
    maybeSubstitutions.map(_.toMap)
  }
}

case class ExpressionId(id: String) extends AnyVal
case class BooleanExprId(id: String) extends AnyVal

case class ExprSubstitutions(expressions: Map[ExpressionId, Expr])

object ExprSubstitutions extends Substitutions[ExpressionId, Expr] {
  val empty = ExprSubstitutions(Map.empty[ExpressionId, Expr])

  override val fieldName = "expressions"

  def toExpression(exprStr: String): Opt[Expr] = {
    val parsed: Opt[ValueExpr] = ValueParser.validate("${" + exprStr + "}")

    parsed match {
      case Right(TextExpression(expr)) => Right(expr)
      case Right(_)                    => Left(UnexpectedState("Wrong expression: " + exprStr))
      case Left(unexpectedState)       => Left(unexpectedState)
    }
  }

  def from(templateRaw: FormTemplateRaw): Opt[ExprSubstitutions] =
    templateRaw.value \ fieldName match {
      case JsDefined(json) =>
        fromJsValue(json)(ExpressionId.apply)(toExpression).map(ExprSubstitutions.apply)
      case JsUndefined() => Right(ExprSubstitutions.empty)
    }
}

object BooleanExprId {
  implicit val format: OFormat[BooleanExprId] = derived.oformat()
}

case class BooleanExprSubstitutions(expressions: Map[BooleanExprId, BooleanExpr])
object BooleanExprSubstitutions extends Substitutions[BooleanExprId, BooleanExpr] {

  override val fieldName = "booleanExpressions"

  val empty = BooleanExprSubstitutions(Map.empty[BooleanExprId, BooleanExpr])

  def toBooleanExpression(booleanExpr: String): Opt[BooleanExpr] = BooleanExprParser.validate("${" + booleanExpr + "}")

  def from(templateRaw: FormTemplateRaw): Opt[BooleanExprSubstitutions] =
    templateRaw.value \ "booleanExpressions" match {
      case JsDefined(json) =>
        fromJsValue(json)(BooleanExprId.apply)(toBooleanExpression).map(BooleanExprSubstitutions.apply)

      case JsUndefined() => Right(BooleanExprSubstitutions.empty)
    }
}
