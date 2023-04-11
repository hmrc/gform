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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.implicits._
import play.api.libs.json.{ JsError, JsString, JsSuccess, OFormat, Reads }
import uk.gov.hmrc.gform.core.parsers.ValueParser

sealed trait OptionDataValue extends Product with Serializable
object OptionDataValue {
  case class StringBased(value: String) extends OptionDataValue
  case class ExprBased(prefix: String, expr: Expr) extends OptionDataValue

  private def readsForTemplateJson: Reads[OptionDataValue] = Reads { json =>
    json match {
      case JsString(exprAsStr) =>
        if (exprAsStr.contains("${") && (exprAsStr.indexOf("${") =!= exprAsStr.lastIndexOf("${"))) {
          throw new IllegalArgumentException(
            s"Invalid expression. Value $exprAsStr has more than one expression. Expected string with only one expression. "
          )
        } else {
          ValueParser
            .validateWithParser(exprAsStr, ValueParser.optionDataValue)
            .fold(e => JsError(e.error), r => JsSuccess(r))
        }
      case otherwise => JsError(s"Invalid expression. Expected string or string with expr, got $otherwise")
    }
  }

  implicit val format: OFormat[OptionDataValue] = OFormatWithTemplateReadFallback(readsForTemplateJson)

  implicit val leafExprs: LeafExpr[OptionDataValue] = (path: TemplatePath, t: OptionDataValue) =>
    t match {
      case StringBased(_)     => Nil
      case ExprBased(_, expr) => List(ExprWithPath(path, expr))
    }
}
