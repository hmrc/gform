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
    multiLine: Boolean,
    dataThreshold: Option[Int],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    maybePrefix: Option[SmartString],
    maybeSuffix: Option[SmartString],
    maybePriority: Option[Priority],
    rows: Int,
    displayCharCount: Boolean,
    json: JsValue
  ): Either[UnexpectedState, ComponentType] =
    (maybeFormatExpr, maybeValueExpr, multiLine, dataThreshold) match {
      case (Some(formatExpr), _, false, None) =>
        createTextObject(
          formatExpr,
          maybeValueExpr,
          maybeDisplayWidth,
          toUpperCase,
          maybePrefix,
          maybeSuffix,
          maybePriority,
          json
        )
      case (Some(formatExpr), _, true, _) =>
        createTextAreaObject(
          formatExpr,
          maybeValueExpr,
          maybeDisplayWidth,
          multiLine,
          dataThreshold,
          rows,
          displayCharCount,
          json
        )
      case _ => createError(maybeFormatExpr, maybeValueExpr, multiLine, json, dataThreshold).asLeft
    }

  def createTextObject(
    formatExpr: FormatExpr,
    maybeValueExpr: Option[ValueExpr],
    maybeDisplayWidth: Option[String],
    toUpperCase: UpperCaseBoolean,
    maybePrefix: Option[SmartString],
    maybeSuffix: Option[SmartString],
    maybePriority: Option[Priority],
    json: JsValue
  ) = (formatExpr, maybeValueExpr, maybeDisplayWidth) match {
    case (TextFormat(f), HasTextExpression(expr), None) =>
      Text(f, expr, DisplayWidth.DEFAULT, toUpperCase, maybePrefix, maybeSuffix, maybePriority).asRight
    case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) =>
      Text(f, expr, dw, toUpperCase, maybePrefix, maybeSuffix, maybePriority).asRight
    case _ => createError(Some(formatExpr), maybeValueExpr, false, json, None).asLeft
  }

  def createTextAreaObject(
    formatExpr: FormatExpr,
    maybeValueExpr: Option[ValueExpr],
    displayWidth: Option[String],
    multiLine: Boolean,
    dataThreshold: Option[Int],
    rows: Int,
    displayCharCount: Boolean,
    json: JsValue
  ) =
    (formatExpr, maybeValueExpr, displayWidth) match {
      case (TextFormat(f), HasTextExpression(expr), None) =>
        TextArea(f, expr, rows = rows, displayCharCount = displayCharCount, dataThreshold = dataThreshold).asRight
      case (TextFormat(f), HasTextExpression(expr), HasDisplayWidth(dw)) =>
        TextArea(f, expr, dw, rows, displayCharCount, dataThreshold).asRight
      case _ => createError(Some(formatExpr), maybeValueExpr, multiLine, json, dataThreshold).asLeft
    }

  def createError(
    maybeFormatExpr: Option[FormatExpr],
    maybeValueExpr: Option[ValueExpr],
    multiLine: Boolean,
    json: JsValue,
    dataThreshold: Option[Int]
  ): UnexpectedState = {

    val formComponentMaker = new FormComponentMaker(json)
    (maybeFormatExpr, maybeValueExpr, multiLine, dataThreshold) match {
      case (maybeInvalidFormat, maybeValue, false, None) =>
        UnexpectedState(s"""|Missing or invalid format for text field
                            |Id: ${formComponentMaker.id}
                            |Format: $maybeInvalidFormat
                            |Value: $maybeValue
                            |""".stripMargin)
      case (_, _, false, Some(_)) =>
        UnexpectedState(s"'dataThreshold' only applies to multline text area for id: ${formComponentMaker.id}")
      case (maybeInvalidFormat, maybeValue, true, _) =>
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
}
