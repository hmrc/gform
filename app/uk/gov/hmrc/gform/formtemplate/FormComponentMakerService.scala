/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ComponentType, DisplayWidth, Expr, FormatExpr, IsNotUpperCase, IsUpperCase, Text, TextArea, TextExpression, TextFormat, UkSortCode, UkSortCodeFormat, UpperCaseBoolean, Value, ValueExpr }
import cats.syntax.either._

object FormComponentMakerService {

  def createObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    json: JsValue): Either[UnexpectedState, ComponentType] =
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      // format: off
      case (Some(TextFormat(UkSortCodeFormat)), HasTextExpression(expr), IsNotMultiline()) => UkSortCode(expr).asRight
      case (Some(formatExpr), _, IsNotMultiline()) => createTextObject(formatExpr, maybeValueExpr, maybeDisplayWidth, toUpperCase)
      case (Some(formatExpr), _, IsMultiline())    => createTextAreaObject(formatExpr, maybeValueExpr, maybeDisplayWidth)
      case _                        => createError(maybeFormatExpr, maybeValueExpr, multiLine, json).asLeft
      // format: on
    }

  def createTextObject(
    formatExpr: FormatExpr,
    maybeValueExpr: Option[ValueExpr],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean) = (formatExpr, maybeValueExpr, maybeDisplayWidth) match {
    // format: off
    case (TextFormat(f), HasTextExpression(expr), None)                => Text(f, expr, DisplayWidth.DEFAULT, toUpperCase).asRight
    case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) => Text(f, expr, dw, toUpperCase).asRight
    // format: on
  }

  def createTextAreaObject(formatExpr: FormatExpr, maybeValueExpr: Option[ValueExpr], displayWidth: Option[String]) =
    (formatExpr, maybeValueExpr, displayWidth) match {
      // format: off
    case (TextFormat(f), HasTextExpression(expr), None)                => TextArea(f, expr).asRight
    case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) => TextArea(f, expr, dw).asRight
    // format: on
    }

  def createError(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    json: JsValue): UnexpectedState = {
    val formComponentMaker = new FormComponentMaker(json)
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      case (maybeInvalidFormat, maybeValue, IsNotMultiline()) =>
        UnexpectedState(s"""|Missing format for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeValue
                            |""".stripMargin)
      case (maybeInvalidFormat, maybeValue, IsMultiline()) =>
        UnexpectedState(s"""|Missing format for multiline text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeValue
                            |""".stripMargin)
    }
  }

  final object HasTextExpression {
    def unapply(valueExp: Option[ValueExpr]): Option[Expr] =
      valueExp match {
        case Some(TextExpression(expr)) => Some(expr)
        case None                       => Some(Value)
        case _                          => None
      }
  }

  final object IsNotMultiline {
    def unapply(multiline: Option[String]): Boolean = !IsMultiline.unapply(multiline)
  }

  final object HasDisplayWidth {
    def unapply(displayWidth: Option[String]): Option[DisplayWidth] =
      displayWidth match {
        case Some("xs")  => Some(DisplayWidth.XS)
        case Some("s")   => Some(DisplayWidth.S)
        case Some("m")   => Some(DisplayWidth.M)
        case Some("l")   => Some(DisplayWidth.L)
        case Some("xl")  => Some(DisplayWidth.XL)
        case Some("xxl") => Some(DisplayWidth.XXL)
        case _           => Some(DisplayWidth.DEFAULT)
      }
  }

  final object ToUpperCase {
    def unapply(isUpperCase: Option[String]): Option[UpperCaseBoolean] =
      isUpperCase match {
        case Some(IsTrueish()) => Some(IsUpperCase)
        case _                 => Some(IsNotUpperCase)
      }
  }

  final object IsMultiline {
    def unapply(multiline: Option[String]): Boolean =
      multiline match {
        case Some(IsTrueish()) => true
        case _                 => false
      }
  }

  object IsTrueish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "true" | "yes" => true
        case _              => false
      }
  }

  object IsFalseish {
    def unapply(maybeBoolean: String): Boolean =
      maybeBoolean.toLowerCase match {
        case "false" | "no" => true
        case _              => false
      }
  }
}
