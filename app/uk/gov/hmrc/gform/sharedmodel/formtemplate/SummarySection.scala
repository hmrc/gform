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

import cats.data.NonEmptyList
import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }

case class SummarySection(
  title: SmartString,
  caption: Option[SmartString],
  header: SmartString,
  footer: SmartString,
  continueLabel: Option[SmartString],
  fields: Option[NonEmptyList[FormComponent]],
  displayWidth: LayoutDisplayWidth.LayoutDisplayWidth = LayoutDisplayWidth.M,
  includeIf: Option[IncludeIf],
  pdf: Option[PdfCtx]
)

object SummarySection extends JsonUtils {

  implicit val leafExprs: LeafExpr[SummarySection] = (path: TemplatePath, t: SummarySection) =>
    LeafExpr(path + "title", t.title) ++
      LeafExpr(path + "caption", t.caption) ++
      LeafExpr(path + "header", t.header) ++
      LeafExpr(path + "footer", t.footer) ++
      LeafExpr(path + "continueLabel", t.continueLabel) ++
      LeafExpr(path + "fields", t.fields) ++
      LeafExpr(path + "includeIf", t.includeIf) ++
      LeafExpr(path + "pdf", t.pdf)

  def defaultJson(formCategory: FormCategory): JsValue = {

    val categoryEn = formCategory.localised(LangADT.En)
    val categoryCy = formCategory.localised(LangADT.Cy)

    Json.obj(
      "title" -> Json.obj(
        "en" -> "Check your answers",
        "cy" -> "Gwiriwch eich atebion"
      ),
      "header" -> Json.obj(
        "en" -> "Make sure the information you have given is correct.",
        "cy" -> "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir."
      ),
      "footer" -> Json.obj(
        "en" -> s"##Now send your $categoryEn\n\nYou need to submit your $categoryEn on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)]($${link.printSummaryPdf}).",
        "cy" -> s"##Nawr anfonwch eich $categoryCy\n\nMae angen i chi gyflwyno’ch $categoryCy ar y sgrin nesaf.\n\nCyn i chi wneud hyn gallwch [argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)]($${link.printSummaryPdf})."
      )
    )
  }

  val jsWithDefaults = Json.using[Json.WithDefaultValues]
  implicit val format: OFormat[SummarySection] = jsWithDefaults.format[SummarySection]
}
