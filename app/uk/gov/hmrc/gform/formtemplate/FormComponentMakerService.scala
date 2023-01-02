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

import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.syntax.either._
import uk.gov.hmrc.gform.sharedmodel.SmartString

object FormComponentMakerService {

  def createObject(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    maybePrefix: Option[SmartString],
    maybeSuffix: Option[SmartString],
    rows: Int,
    displayCharCount: Boolean,
    json: JsValue
  ): Either[UnexpectedState, ComponentType] =
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      case (Some(formatExpr), _, IsNotMultiline()) =>
        createTextObject(formatExpr, maybeValueExpr, maybeDisplayWidth, toUpperCase, maybePrefix, maybeSuffix, json)
      case (Some(formatExpr), _, IsMultiline()) =>
        createTextAreaObject(formatExpr, maybeValueExpr, maybeDisplayWidth, multiLine, rows, displayCharCount, json)
      case _ => createError(maybeFormatExpr, maybeValueExpr, multiLine, json).asLeft
    }

  def createTextObject(
    formatExpr: FormatExpr,
    maybeValueExpr: Option[ValueExpr],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    maybePrefix: Option[SmartString],
    maybeSuffix: Option[SmartString],
    json: JsValue
  ) = (formatExpr, maybeValueExpr, maybeDisplayWidth) match {
    case (TextFormat(f), HasTextExpression(expr), None) =>
      Text(f, expr, DisplayWidth.DEFAULT, toUpperCase, maybePrefix, maybeSuffix).asRight
    case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) =>
      Text(f, expr, dw, toUpperCase, maybePrefix, maybeSuffix).asRight
    case _ => createError(Some(formatExpr), maybeValueExpr, None, json).asLeft
  }

  def createTextAreaObject(
    formatExpr: FormatExpr,
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String],
    multiLine: Option[String],
    rows: Int,
    displayCharCount: Boolean,
    json: JsValue
  ) =
    (formatExpr, maybeValueExpr, displayWidth) match {
      case (TextFormat(f), HasTextExpression(expr), None) =>
        TextArea(f, expr, rows = rows, displayCharCount = displayCharCount).asRight
      case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) =>
        TextArea(f, expr, dw, rows, displayCharCount).asRight
      case _ => createError(Some(formatExpr), maybeValueExpr, multiLine, json).asLeft
    }

  def createError(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Option[String],
    json: JsValue
  ): UnexpectedState = {
    val formComponentMaker = new FormComponentMaker(json)
    (maybeFormatExpr, maybeValueExpr, multiLine) match {
      case (maybeInvalidFormat, maybeValue, IsNotMultiline()) =>
        UnexpectedState(s"""|Missing or invalid format for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeValue
                            |""".stripMargin)
      case (maybeInvalidFormat, maybeValue, IsMultiline()) =>
        UnexpectedState(s"""|Missing or invalid format for multiline text field
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

  final object IsDisplayCharCountFalse {
    def unapply(displayCharCount: Option[String]): Boolean =
      displayCharCount match {
        case Some(IsFalseish()) => true
        case _                  => false
      }
  }

  final object IsDisplayCharCountTrue {
    def unapply(displayCharCount: Option[String]): Boolean =
      !IsDisplayCharCountFalse.unapply(displayCharCount)
  }
}
