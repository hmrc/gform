/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate

import cats.implicits._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.libs.json.Json

class HtmlValidatorResult(
  val invalidHtml: String,
  val possibleFix: String
) {
  def asJson = Json.obj(
    // Keys are intentionally of identical length for better readability
    "invalidHtml" -> invalidHtml,
    "possibleFix" -> possibleFix
  )
}

object HtmlValidator {

  def validate(translatableRows: List[(String, String)]): Option[HtmlValidatorResult] =
    translatableRows
      .flatMap { case (english, welsh) =>
        List(isValidHtml(english), isValidHtml(welsh))
      }
      .flatten
      .headOption // Let's return first error only

  private def isValidHtml(text: String): Option[HtmlValidatorResult] = {
    val textForParsing = text
      .replaceAll(
        "&nbsp;",
        ""
      ) // Remove non breaking space entity so the following replaceAll is not going to interere with it
      .replaceAll("&gt;", "")
      .replaceAll("&lt;", "")
      .replaceAll("\\$\\{[^}]*\\}", "\\$\\{redacted\\}")

    val document: Document = Jsoup.parseBodyFragment(textForParsing)

    document.outputSettings().prettyPrint(false)

    val htmlOutput: String = document.body.html()

    val sanitizedText = textForParsing
      // We need to replace ' with " for comparison purposes because Jsoup prints html always with "
      .replaceAll("'", "\"")
      // Jsoup doesn't preserve spaces
      .replaceAll(" ", "")
      // Jsoup always print breaks as <br>
      .replaceAll("<br/>", "<br>")
      // Jsoup is escaping free standing &
      .replaceAll("&", "&amp;")

    val sanitizedHtmlOutput = htmlOutput
      .replaceAll("'", "\"")
      .replaceAll(" ", "")

    val isValidRoundTrip = sanitizedText === sanitizedHtmlOutput

    if (isValidRoundTrip) {
      None
    } else {
      val possibleFix = htmlOutput
        .replaceAll("\"", "'")
        .replaceAll("&amp;", "&")
      Some(
        // If original uses ' for html attribute it makes comparison with html htmlOutput hard so we replace all " with ' in resulting json for better debugging
        new HtmlValidatorResult(textForParsing.replaceAll("\"", "'"), possibleFix)
      )
    }
  }

}
