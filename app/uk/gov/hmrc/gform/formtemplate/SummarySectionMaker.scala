/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.data.NonEmptyList
import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IncludeIf, LayoutDisplayWidth, PdfCtx, SummarySection }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth

class SummarySectionMaker(json: JsValue) {
  val title: SmartString = (json \ "title").as[SmartString]
  val caption: Option[SmartString] = (json \ "caption").asOpt[SmartString]
  val header: SmartString = (json \ "header").as[SmartString]
  val footer: SmartString = (json \ "footer").as[SmartString]
  val continueLabel: Option[SmartString] = (json \ "continueLabel").asOpt[SmartString]
  val fields: Option[NonEmptyList[FormComponent]] =
    (json \ "fields").asOpt[List[FormComponent]].flatMap(NonEmptyList.fromList)
  val displayWidth: LayoutDisplayWidth =
    (json \ "displayWidth").asOpt[LayoutDisplayWidth].getOrElse(LayoutDisplayWidth.M)
  val includeIf: Option[IncludeIf] = (json \ "includeIf").asOpt[IncludeIf]
  val pdf: Option[PdfCtx] = (json \ "pdf").asOpt[PdfCtx]

  def readExcludeFromPdf: Opt[Option[List[FormComponentId]]] =
    (json \ "fields").asOpt[List[JsValue]] match {
      case Some(fieldList) =>
        val formComponentIds = fieldList.flatMap { value =>
          (value \ "excludeFromPdf")
            .asOpt[Boolean]
            .collect { case true =>
              (value \ "id").asOpt[FormComponentId]
            }
            .flatten
        }
        Right(Option(formComponentIds).filter(_.nonEmpty))
      case None =>
        Right(None)
    }

  def optSummarySection(): Opt[SummarySection] =
    for {
      excludeFromPdf <- readExcludeFromPdf
    } yield SummarySection(
      title,
      caption,
      header,
      footer,
      continueLabel,
      fields,
      displayWidth,
      includeIf,
      pdf,
      excludeFromPdf
    )
}
