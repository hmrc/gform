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

package uk.gov.hmrc.gform.submission.destinations

import java.util.UUID
import play.api.libs.json.{ JsNumber, JsObject, JsResult, JsString, JsValue, OFormat }
import uk.gov.hmrc.gform.sharedmodel.PdfContent

case class SummaryHtml(id: SummaryHtmlId, summaryHtml: PdfContent)

object SummaryHtml {
  implicit val format: OFormat[SummaryHtml] = new OFormat[SummaryHtml] {
    override def reads(json: JsValue): JsResult[SummaryHtml] =
      for {
        id          <- (json \ "id").validate[UUID]
        summaryHtml <- (json \ "summaryHtml").validate[String]
      } yield SummaryHtml(SummaryHtmlId(id), PdfContent(summaryHtml))

    override def writes(summary: SummaryHtml): JsObject = {
      import summary._

      JsObject(
        Seq(
          "id"          -> JsString(id.value.toString),
          "hash"        -> JsNumber(summaryHtml.hashCode),
          "summaryHtml" -> JsString(summaryHtml.content)
        )
      )
    }
  }
}
