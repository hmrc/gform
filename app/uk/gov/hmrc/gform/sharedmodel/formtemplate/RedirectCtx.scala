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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.functional.syntax._
import play.api.libs.json._

final case class RedirectCtx(
  ifExpr: Option[IncludeIf],
  redirectUrl: String
)

object RedirectCtx {
  private val reads: Reads[RedirectCtx] =
    ((JsPath \ "if")
      .readNullable[IncludeIf] and
      (JsPath \ "redirectUrl").read[String])(RedirectCtx.apply _)

  private val writes: Writes[RedirectCtx] =
    ((JsPath \ "if").writeNullable[IncludeIf] and
      (JsPath \ "redirectUrl").write[String])(unlift(RedirectCtx.unapply))

  implicit val format: Format[RedirectCtx] = Format[RedirectCtx](reads, writes)

  implicit val leafExprs: LeafExpr[RedirectCtx] = (path: TemplatePath, r: RedirectCtx) =>
    LeafExpr(path + "if", r.ifExpr)
}
