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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, LeafExpr, TemplatePath }

sealed trait PrintSection extends Product with Serializable

object PrintSection {
  case class Page(title: SmartString, instructions: SmartString)

  object Page {
    implicit val format: OFormat[Page] = Json.format[Page]

    implicit val leafExprs: LeafExpr[Page] = (path: TemplatePath, t: Page) =>
      LeafExpr(path + "title", t.title) ++
        LeafExpr(path + "instructions", t.instructions)

  }

  case class Pdf(header: SmartString, footer: SmartString)

  object Pdf {
    implicit val format: OFormat[Pdf] = Json.format[Pdf]
    implicit val leafExprs: LeafExpr[Pdf] = (path: TemplatePath, t: Pdf) =>
      LeafExpr(path + "header", t.header) ++
        LeafExpr(path + "footer", t.footer)

  }

  case class PdfNotification(header: SmartString, footer: SmartString, fieldIds: List[FormComponentId])

  object PdfNotification {
    implicit val format: OFormat[PdfNotification] = Json.format[PdfNotification]

    implicit val leafExprs: LeafExpr[PdfNotification] = (path: TemplatePath, t: PdfNotification) =>
      LeafExpr(path + "header", t.header) ++
        LeafExpr(path + "footer", t.footer) ++
        LeafExpr(path + "fieldIds", t.fieldIds)

  }
}
