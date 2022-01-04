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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.BooleanExpr

package object booleanParser {
  def booleanExprParser(json: JsValue): JsResult[BooleanExpr] =
    json match {
      case JsString(exprAsStr) => parse(exprAsStr)
      case otherwise           => JsError(s"Invalid expression. Expected String, got $otherwise")
    }

  def parse(exprAsStr: String): JsResult[BooleanExpr] =
    BooleanExprParser.validate(exprAsStr) fold (error => JsError(error.toString),
    expr => JsSuccess(expr))
}
