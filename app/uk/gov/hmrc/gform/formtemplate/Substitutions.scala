/*
 * Copyright 2023 HM Revenue & Customs
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
import play.api.libs.json.{ JsDefined, JsError, JsObject, JsString, JsSuccess, JsUndefined, JsValue, OFormat, Reads }
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ADTFormat, BooleanExpr, ExplicitExprType, Expr, FormComponentId, FormTemplateRaw, RoundingMode, TextConstraint, TextExpression, Typed, ValueExpr }
import uk.gov.hmrc.gform.core.{ Invalid, Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState

sealed trait Substitutions[K, V] {

  def fieldName: String

  val typedReads: Reads[Typed] = {
    val textReads: Reads[ExplicitExprType.Text.type] = Reads.pure(ExplicitExprType.Text)
    val sterlingReads: Reads[ExplicitExprType.Sterling] = Reads { json =>
      for {
        roundingMode <- (json \ "round").validateOpt[RoundingMode]
      } yield ExplicitExprType.Sterling(roundingMode.getOrElse(RoundingMode.Down))
    }
    val numberReads: Reads[ExplicitExprType.Number] = Reads { json =>
      for {
        fractionalDigits <- (json \ "fractionalDigits").validateOpt[Int]
        roundingMode     <- (json \ "round").validateOpt[RoundingMode]
      } yield ExplicitExprType.Number(
        fractionalDigits.getOrElse(TextConstraint.defaultFractionalDigits),
        roundingMode.getOrElse(RoundingMode.defaultRoundingMode)
      )
    }

    val exprTypeReads: Reads[ExplicitExprType] = ADTFormat.adtRead[ExplicitExprType](
      "type",
      "text"     -> textReads,
      "number"   -> numberReads,
      "sterling" -> sterlingReads
    )

    Reads { json =>
      for {
        expr <- (json \ "value").validate[String].flatMap { expr =>
                  ExprSubstitutions.toExpression(expr) match {
                    case Left(invalid) => JsError(invalid.error)
                    case Right(expr)   => JsSuccess(expr)
                  }
                }
        tpe <- json.validate[ExplicitExprType](exprTypeReads)
      } yield Typed(expr, tpe)
    }
  }

  def fromJsValue(
    jsValue: JsValue
  )(toKey: String => K)(f: String => Opt[V])(g: JsObject => Opt[V]): Opt[Map[K, V]] = {

    val maybeSubstitutions: Opt[List[(K, V)]] =
      jsValue match {
        case JsObject(fields) =>
          fields.toList.traverse { case (k, v) =>
            val maybeExpr: Opt[V] = v match {
              case JsString(exprStr) => f(exprStr)
              case obj @ JsObject(_) => g(obj)
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
object ExpressionId {
  implicit val format: OFormat[ExpressionId] = derived.oformat()
}
case class BooleanExprId(id: String) extends AnyVal

class SpecimenExprSubstitutions extends ExprSubstitutions(Map.empty[ExpressionId, Expr])
case class ExprSubstitutions(expressions: Map[ExpressionId, Expr]) {
  def resolveSelfReferences: Either[UnexpectedState, ExprSubstitutions] =
    TopLevelExpressions.resolveReferences(this)
}

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

  def toExpressionFromObj(obj: JsObject): Opt[Expr] =
    obj.validate[Typed](typedReads) match {
      case JsError(error)     => Left(UnexpectedState(JsError.toJson(error).toString()))
      case JsSuccess(expr, _) => Right(expr)
    }

  private def validateIds(
    optExprSubstitutions: Either[UnexpectedState, ExprSubstitutions]
  ): Either[UnexpectedState, ExprSubstitutions] = {
    val validationResult: ValidationResult = optExprSubstitutions
      .map { substitutions =>
        substitutions.expressions
          .map { case (id, expr) =>
            val fcIds: List[FormComponentId] = TopLevelExpressions.resolveFormComponentIds(expr)
            if (fcIds.exists(_.value == id.id)) Invalid(s"The expression ${id.id} cannot reference itself")
            else Valid
          }
          .toList
          .combineAll
      }
      .getOrElse(Valid)

    validationResult match {
      case Valid          => optExprSubstitutions
      case Invalid(error) => Left(UnexpectedState(error))
    }
  }

  def from(templateRaw: FormTemplateRaw): Opt[ExprSubstitutions] =
    templateRaw.value \ fieldName match {
      case JsDefined(json) =>
        val optExprSubstitutions =
          fromJsValue(json)(ExpressionId.apply)(toExpression)(toExpressionFromObj).map(ExprSubstitutions.apply)
        validateIds(optExprSubstitutions)
      case _: JsUndefined => Right(ExprSubstitutions.empty)
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

  private def validateExprIds(
    optBooleanExprSubstitutions: Either[UnexpectedState, BooleanExprSubstitutions]
  ): Either[UnexpectedState, BooleanExprSubstitutions] = {
    val validationResult: ValidationResult = optBooleanExprSubstitutions
      .map { substitutions =>
        substitutions.expressions
          .map { case (id, expr) =>
            val booleanExprIds: List[BooleanExprId] = BooleanExpr.resolveBooleanExprIds(expr)
            if (booleanExprIds.contains(id)) Invalid(s"The booleanExpression ${id.id} cannot reference itself")
            else Valid
          }
          .toList
          .combineAll
      }
      .getOrElse(Valid)

    validationResult match {
      case Valid          => optBooleanExprSubstitutions
      case Invalid(error) => Left(UnexpectedState(error))
    }
  }
  def from(templateRaw: FormTemplateRaw): Opt[BooleanExprSubstitutions] =
    templateRaw.value \ "booleanExpressions" match {
      case JsDefined(json) =>
        val optBooleanExprSubstitutions: Either[UnexpectedState, BooleanExprSubstitutions] =
          fromJsValue(json)(BooleanExprId.apply)(toBooleanExpression)(obj =>
            Left(
              UnexpectedState(
                s"Expected JsString, but got " + obj.getClass.getSimpleName + ": " + obj
              )
            )
          ).map(BooleanExprSubstitutions.apply)
        validateExprIds(optBooleanExprSubstitutions)

      case _: JsUndefined => Right(BooleanExprSubstitutions.empty)
    }
}
