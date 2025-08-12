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

import play.api.libs.json.{ JsError, JsSuccess, Json, OFormat, Reads }
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.core.parsers.ValueParser

final case class Confirmation(
  question: FormComponent,
  redirects: Option[NonEmptyList[ConfirmationRedirect]],
  fieldsConfirmed: Option[NonEmptyList[FormComponentId]],
  expressionsConfirmed: Option[NonEmptyList[Expr]]
)

object Confirmation {
  import JsonUtils._
  val exprReads: Reads[Expr] = Reads { json =>
    Reads.of[String].reads(json).flatMap { expression =>
      ValueParser
        .validateWithParser(expression, ValueParser.expr)
        .fold(unexpectedState => JsError(unexpectedState.error), JsSuccess(_))
    }
  }

  implicit val exprFormat: OFormat[Expr] = OFormatWithTemplateReadFallback(exprReads)
  implicit val confirmationFormat: OFormat[Confirmation] = Json.format[Confirmation]

  implicit val leafExprs: LeafExpr[Confirmation] = (path: TemplatePath, t: Confirmation) =>
    LeafExpr(path + "question", t.question) ++
      LeafExpr(path + "redirects", t.redirects) ++
      LeafExpr(path + "fieldsConfirmed", t.fieldsConfirmed) ++
      LeafExpr(path + "expressionsConfirmed", t.expressionsConfirmed)

}
