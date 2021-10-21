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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.{ JsError, JsString, JsSuccess, OFormat, Reads }
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.exceptions.UnexpectedState

final case class AddToListLimit(
  repeatsMax: Expr,
  field: FormComponent
)

object AddToListLimit {

  val readsExpr: Reads[Expr] = Reads {
    case JsString(str) =>
      ValueParser.validate(str) match {
        case Right(TextExpression(expr))  => JsSuccess(expr)
        case Right(_)                     => JsError("Expected an expression, got " + str)
        case Left(UnexpectedState(error)) => JsError("Error, expected valid expression, got " + error)
      }
    case unknown => JsError("Expected JsString, got " + unknown)
  }

  implicit val format: OFormat[AddToListLimit] =
    OFormatWithTemplateReadFallback {
      implicit val abc = readsExpr
      derived.reads[AddToListLimit]()
    }

  implicit val leafExprs: LeafExpr[AddToListLimit] = (path: TemplatePath, t: AddToListLimit) =>
    LeafExpr(path + "repeatsMax", t.repeatsMax) ++
      LeafExpr(path + "field", t.field)
}
