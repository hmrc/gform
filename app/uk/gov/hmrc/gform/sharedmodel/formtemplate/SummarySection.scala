/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json.{ JsValue, Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }

case class SummarySection(
  title: SmartString,
  header: SmartString,
  footer: SmartString
)

object SummarySection {

  def defaultJson(formCategory: FormCategory): JsValue = {

    val categoryEn = formCategory.localised(LangADT.En)
    val categoryCy = formCategory.localised(LangADT.Cy)

    Json.obj(
      "title" -> Json.obj(
        "en" -> "Check your answers",
        "cy" -> "Gwiriwch eich atebion"
      ),
      "header" -> Json.obj(
        "en" -> "Make sure the information you have given is correct",
        "cy" -> "Gwnewch yn siŵr bod yr wybodaeth a roddwyd gennych yn gywir"
      ),
      "footer" -> Json.obj(
        "en" -> s"##Now send your $categoryEn\n\nYou need to submit your $categoryEn on the next screen.\n\nBefore you do this you can [print or save a PDF copy of your answers (opens in a new window or tab)](/submissions/summary/pdf/$${form.id}).",
        "cy" -> s"##Nawr anfonwch eich $categoryCy\n\nMae angen i chi gyflwyno’ch $categoryCy ar y sgrin nesaf.\n\nCyn i chi wneud hyn, [gallwch argraffu neu gadw copi PDF o’ch atebion (yn agor ffenestr neu dab newydd)](/submissions/summary/pdf/$${form.id})."
      )
    )
  }

  implicit val format: OFormat[SummarySection] = Json.format[SummarySection]
}
