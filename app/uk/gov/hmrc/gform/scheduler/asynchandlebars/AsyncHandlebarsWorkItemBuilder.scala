/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.asynchandlebars

import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel, TemplateType }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, HandlebarsTemplateProcessor }

object AsyncHandlebarsWorkItemBuilder {

  def createRenderSnapshot(
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): AsyncHandlebarsRenderSnapshot =
    AsyncHandlebarsRenderSnapshot(
      accumulatedModel = accumulatedModel,
      model = modelTree.value.model,
      pdfData = modelTree.value.pdfData,
      instructionPdfData = modelTree.value.instructionPdfData,
      structuredFormData = modelTree.value.structuredFormData,
      formId = modelTree.value.formId,
      submissionRef = modelTree.value.submissionRef
    )

  def build(
    destination: Destination.AsyncHandlebarsHttpApi,
    destinationHttpHeaders: Map[String, String],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    renderSnapshot: Option[AsyncHandlebarsRenderSnapshot],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor
  ): AsyncHandlebarsWorkItem = {
    val uri = handlebarsTemplateProcessor(
      destination.uri,
      accumulatedModel,
      FocussedHandlebarsModelTree(modelTree),
      TemplateType.Plain
    )

    def processPayload(template: String): String =
      handlebarsTemplateProcessor(
        template,
        accumulatedModel,
        FocussedHandlebarsModelTree(modelTree),
        destination.payloadType
      )

    AsyncHandlebarsWorkItem(
      profile = destination.profile,
      uri = uri,
      method = destination.method,
      contentType = contentType(destination.payloadType),
      payload = destination.payload.fold("")(processPayload),
      credential = destination.credential,
      httpHeaders = destinationHttpHeaders,
      renderSnapshot = renderSnapshot
    )
  }

  private def contentType(templateType: TemplateType): ContentType =
    templateType match {
      case TemplateType.JSON  => ContentType.`application/json`
      case TemplateType.XML   => ContentType.`application/xml`
      case TemplateType.Plain => ContentType.`text/plain`
    }
}
