/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BasicText, ComponentType, DisplayWidth, Expr, FormatExpr, IsNotUpperCase, IsUpperCase, ShortText, Text, TextArea, TextExpression, TextFormat, UkSortCode, UkSortCodeFormat, UpperCaseBoolean, Value, ValueExpr }
import cats.syntax.either._

object FormComponentMakerService {

  def createObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    displayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    json: JsValue): Either[UnexpectedState, ComponentType] =
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      // format: off
      case (Some(TextFormat(UkSortCodeFormat)), HasTextExpression(expr), IsNotMultiline()) => UkSortCode(expr).asRight
      case (_, _, IsNotMultiline()) => createTextObject(maybeFormatExpr, maybeValueExpr, displayWidth, toUpperCase)
      case (_, _, IsMultiline())    => createTextAreaObject(maybeFormatExpr, maybeValueExpr, displayWidth)
      case _                        => createError(maybeFormatExpr, maybeValueExpr, multiLine, json).asLeft
      // format: on
    }

  def createTextObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String],
    toUpperCase: UpperCaseBoolean) = (maybeFormatExpr, maybeValueExpr, displayWidth) match {
    // format: off
    case (Some(TextFormat(f)), HasTextExpression(expr), None)                => Text(f, expr, DisplayWidth.DEFAULT, toUpperCase).asRight
    case (None,                HasTextExpression(expr), None)                => Text(ShortText, expr, DisplayWidth.DEFAULT, toUpperCase).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw)) => Text(f, expr, dw, toUpperCase).asRight
    case (None,                HasTextExpression(expr), HasDisplayWidth(dw)) => Text(ShortText, expr, dw, toUpperCase).asRight
    // format: on
  }

  def createTextAreaObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String]) = (maybeFormatExpr, maybeValueExpr, displayWidth) match {
    // format: off
    case (Some(TextFormat(f)), HasTextExpression(expr), None)                => TextArea(f, expr).asRight
    case (None,                HasTextExpression(expr), None)                => TextArea(BasicText, expr).asRight
    case (Some(TextFormat(f)), HasTextExpression(expr), HasDisplayWidth(dw)) => TextArea(f, expr, dw).asRight
    case (None,                HasTextExpression(expr), HasDisplayWidth(dw)) => TextArea(BasicText, expr, dw).asRight
    // format: on
  }

  def createError(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    json: JsValue): UnexpectedState = {
    val formComponentMaker = new FormComponentMaker(json)
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      case (maybeInvalidFormat, maybeInvalidValue, IsMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format or value for multiline text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeInvalidValue
                            |""".stripMargin)
      case (Some(invalidFormat), None, IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $invalidFormat
                            |Value: must supply a value
                            |""".stripMargin)
      case (None, Some(invalidValue), IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: "must supply a value for format"
                            |Value: $invalidValue
                            |""".stripMargin)
      case (Some(invalidFormat), Some(invalidValue), IsNotMultiline()) =>
        UnexpectedState(s"""|Unsupported type of format and value for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $invalidFormat
                            |Value: $invalidValue
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
