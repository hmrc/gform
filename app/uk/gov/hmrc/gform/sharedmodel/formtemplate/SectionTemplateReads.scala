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

import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.PresentationHintParser
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.AddToList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat

object SectionTemplateReads {
  def reads: Reads[Section] = new Reads[Section] {
    override def reads(json: JsValue): JsResult[Section] =
      (json \ "type")
        .validateOpt[String]
        .flatMap { _.fold(readWithoutType(json)) { readWithType(_, json) } }

    private def readWithoutType(json: JsValue) =
      if ((json \ "pages").isDefined)
        readAddToList(json)
      else if (hasRepeatingPageFields(json))
        readRepeatingPage(json)
      else
        readNonRepeatingPage(json)

    private def hasRepeatingPageFields(json: JsValue) =
      (json \ "repeatsMin").isDefined ||
        (json \ "repeatsMax").isDefined ||
        (json \ "repeats").isDefined

    private def readWithType(sectionType: String, json: JsValue) = sectionType match {
      case "nonRepeatingPage" => readNonRepeatingPage(json)
      case "repeatingPage"    => readRepeatingPage(json)
      case "addToList"        => readAddToList(json)
    }

    private def readAddToList(json: JsValue) = {
      implicit val presentationHintsReads: OFormat[PresentationHint] =
        OFormatWithTemplateReadFallback {
          case JsString(str) => PresentationHintParser.validateSingle(str).fold(e => JsError(e.error), JsSuccess(_))
          case unknown       => JsError("Expected String, got " + unknown)
        }
      Json.format[AddToList].reads(json)
    }

    private def readNonRepeatingPage(json: JsValue) =
      Page.pageFormat.reads(json).map(Section.NonRepeatingPage)

    private def readRepeatingPage(json: JsValue) =
      for {
        repeats <- readRepeatsRangeOrRepeats(json)
        page    <- Page.pageFormat.reads(json)
      } yield Section.RepeatingPage(page, repeats)
  }

  private def readRepeatsRangeOrRepeats(json: JsValue): JsResult[Expr] =
    for {
      (oRepeatsMin, oRepeatsMax) <- readRepeatsRange(json)
      oRepeats                   <- readRepeats(json)
      result <- (oRepeatsMin, oRepeatsMax, oRepeats) match {
                 case (None, None, Some(repeats))                     => JsSuccess(repeats)
                 case (Some(min), Some(max), None) if min.equals(max) => JsSuccess(min)
                 case _ =>
                   JsError(
                     s"""A "repeatingPage" section must have either "repeatsMin" and "repeatsMax" fields (which must be identical), or a "repeats" field : $json""")
               }
    } yield result

  private def readRepeatsRange(json: JsValue) =
    for {
      repeatsMin <- (json \ "repeatsMin").validateOpt[TextExpression].map(_.map(_.expr))
      repeatsMax <- (json \ "repeatsMax").validateOpt[TextExpression].map(_.map(_.expr))
    } yield (repeatsMin, repeatsMax)

  private def readRepeats(json: JsValue) = (json \ "repeats").validateOpt[Expr]
}
