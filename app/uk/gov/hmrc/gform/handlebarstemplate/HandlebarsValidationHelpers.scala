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

package uk.gov.hmrc.gform.handlebarstemplate

import com.github.jknack.handlebars.{ Handlebars, TagType, Template }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ HandlebarsTemplateProcessorModel, UploadableConditioning }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ PdfContent, SubmissionRef }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, HandlebarsTemplateProcessorHelpers, RecursiveHandlebarsTemplateProcessor }

import scala.jdk.CollectionConverters._

object HandlebarsValidationHelpers {

  def validateHandlebarsPayload(payload: String, formTemplate: FormTemplate): Set[String] = {
    val handlebars: Handlebars = getHandlebarsInstance(formTemplate)
    val conditionedPayload: String = UploadableConditioning.conditionAndValidate(Some(true), payload).getOrElse("")
    val compiledTemplate: Template = handlebars.compileInline(conditionedPayload)

    val allTokensAndVariables: List[String] =
      (compiledTemplate.collect(TagType.VAR, TagType.SUB_EXPRESSION).asScala ++ compiledTemplate
        .collectReferenceParameters()
        .asScala).toList

    val refinedTokensFromPayload: Set[String] = allTokensAndVariables
      .flatMap(_.split("[.-]"))
      .filterNot(_.startsWith("@"))
      .toSet

    val syntheticFields: Set[String] = extractSyntheticTokens(payload)
    val allValidFormFields: Set[String] = extractAllFormFields(formTemplate) ++ syntheticFields
    val knownHelpersAndReservedWords: Set[String] = extractKnownHelpers(handlebars) ++ reservedWords

    // Identify tokens in the payload that are not in the valid tokens set
    refinedTokensFromPayload.diff(allValidFormFields ++ knownHelpersAndReservedWords)
  }

  private def extractSyntheticTokens(payload: String): Set[String] = {
    val tokenPattern = "\\{\\{\\s*#with[^}|]+\\|([a-zA-Z0-9]+)\\|\\s*}}".r
    tokenPattern.findAllMatchIn(payload).map(_.group(1)).toSet
  }

  private val reservedWords: Set[String] =
    Set(
      "this",
      "street1",
      "street2",
      "street3",
      "street4",
      "line1",
      "line2",
      "line3",
      "line4",
      "city",
      "town",
      "postcode",
      "country",
      "year",
      "month",
      "day",
      "revealed",
      "choices",
      "choice",
      "formId",
      "periodKey",
      "periodFrom",
      "periodTo",
      "formStatus",
      "summaryHtml",
      "instructionHtml",
      "submissionReference",
      "caseworker",
      "uploadedFiles",
      "name",
      "extension",
      "data",
      "status",
      "submissionRef",
      "formBundle",
      "tree",
      "submissionRefs",
      "user",
      "enrolledIdentifier",
      "customerId"
    )

  private def extractAllFormFields(formTemplate: FormTemplate): Set[String] =
    (
      formTemplate.formKind.allSections.flatMap { section =>
        section.fold { page =>
          page.page.allFormComponentIds.map(_.value)
        } { repeatingPage =>
          repeatingPage.page.allFormComponentIds.map(_.value)
        } { addToList =>
          addToList.addAnotherQuestion.id.value :: addToList.pages.toList.flatMap(
            _.allFormComponentIds.map(_.value)
          )
        }
      } ++ formTemplate.expressionsOutput.fold(List.empty[String])(
        _.lookup.keys.map(_.id).toList
      ) ++ formTemplate.destinations.allFormComponents.map(_.id.value)
    ).toSet

  private def extractKnownHelpers(handlebars: Handlebars): Set[String] =
    handlebars
      .helpers()
      .asScala
      .map(_.getKey)
      .filterNot(_.contains("$"))
      .toSet

  private def getHandlebarsInstance(formTemplate: FormTemplate): Handlebars = {
    val emptyTree: HandlebarsModelTree = HandlebarsModelTree(
      FormId(""),
      SubmissionRef(""),
      formTemplate,
      PdfContent(""),
      None,
      StructuredFormValue.ObjectStructure(List.empty),
      HandlebarsTemplateProcessorModel.empty
    )

    val helpers: HandlebarsTemplateProcessorHelpers =
      new HandlebarsTemplateProcessorHelpers(
        HandlebarsTemplateProcessorModel.empty,
        emptyTree,
        new DummyTemplateProcessor
      )

    new Handlebars().registerHelpers(helpers)
  }

  private class DummyTemplateProcessor extends RecursiveHandlebarsTemplateProcessor {
    override def apply(
      template: String,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      focussedTree: FocussedHandlebarsModelTree
    ): String = template
  }

}
