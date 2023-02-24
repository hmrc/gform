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

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.SmartString

case class PdfCtx(
  header: Option[SmartString],
  footer: Option[SmartString],
  tabularFormat: Option[Boolean],
  includeSignatureBox: Option[Boolean]
)

object PdfCtx {
  implicit val format: OFormat[PdfCtx] = Json.format[PdfCtx]

  implicit val leafExprs: LeafExpr[PdfCtx] = (path: TemplatePath, t: PdfCtx) =>
    LeafExpr(path + "header", t.header) ++
      LeafExpr(path + "footer", t.footer)
}
