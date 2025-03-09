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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.core.{ FOpt, fromOptA }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.implicits._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.sharedmodel.HandlebarsSchemaId

import scala.concurrent.ExecutionContext

trait Verifier {
  def verify(
    formTemplate: FormTemplate,
    appConfig: AppConfig,
    handlebarsSchemaIds: List[HandlebarsSchemaId]
  )(expressionsContext: ExprSubstitutions)(implicit ec: ExecutionContext): FOpt[Unit] = {

    val sections = formTemplate.formKind.allSections

    val pages = SectionHelper.pages(sections)

    val formComponents: List[FormComponent] = pages.flatMap(_.fields) ++ formTemplate.destinations.allFormComponents
    val componentTypes: List[ComponentType] = formComponents.map(_.`type`)

    val languages = formTemplate.languages

    val allExpressions: List[ExprWithPath] = LeafExpr(TemplatePath.root, formTemplate)

    val expressionIds: List[ExpressionId] = expressionsContext.expressions.keys.toList

    for {
      _ <- fromOptA(FormTemplateValidator.validateLowercaseIds(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateLanguages(languages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceHelpText(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceHints(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceDividerPositionLowerBound(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceDividerPositionUpperBound(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceDividerPositionValue(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceNoneChoiceAndError(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceNoneChoiceMultivalueOnly(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceNoneChoiceValue(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceNoneChoiceLowerBound(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceNoneChoiceUpperBound(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceOptions(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateRevealingChoiceOptions(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniqueFields(sections, expressionIds).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniquePageIds(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateForwardReference(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validate(componentTypes, formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDependencyGraph(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEnrolmentSection(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateRegimeId(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEmailParameter(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEnrolmentIdentifier(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDates(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateGroup(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateRevealingChoice(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateRevealingChoiceHint(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateEmailVerification(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateInstructions(pages).toEither)
      _ <- fromOptA(
             FormTemplateValidator.validateInvalidReferences(formTemplate, allExpressions, expressionIds).toEither
           )
      _ <- fromOptA(FormTemplateValidator.validateReferencesConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateErrorMessageConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateNoPIITitleConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddressReferencesConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validatePeriodFunReferenceConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateBetweenFunReferenceConstraints(formTemplate, allExpressions).toEither)
      _ <- fromOptA(
             FormTemplateValidator.validateDateFunctionReferenceConstraints(formTemplate, allExpressions).toEither
           )
      _ <- fromOptA(FormTemplateValidator.validateDateConstructExpressions(allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateSectionShortNames(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateSummarySection(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddToListCYAPage(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddToListDefaultPage(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddToListInfoFields(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddToListAddAnotherQuestion(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateAddToListRepeatConfig(formTemplate, pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDataRetrieve(formTemplate, pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDataRetrieveForwardReferences(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDataRetrieveCtx(formTemplate, pages, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDataRetrieveCount(formTemplate, pages, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validatePagesToRevisit(sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateConfirmations(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceSize(pages, allExpressions).toEither)
      _ <- fromOptA(FormTemplateValidator.validatePostcodeLookup(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateLabel(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateSubmitSection(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateCsvCountryCountCheck(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validatePageRedirects(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateTaskListDisplayWidth(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateTaskListDeclarationSection(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDataThreshold(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateFileUpload(formTemplate, appConfig).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoicesRevealedField(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceFormCtxOptionValues(pages, formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniqueTaskIds(formTemplate).toEither)
      _ <- fromOptA(FormTemplateValidator.validateIncludeIfForTaskStatus(formTemplate, sections).toEither)
      _ <- fromOptA(FormTemplateValidator.validateDraftRetrieval(formTemplate).toEither)
      _ <- fromOptA(DestinationsValidator.validateUniqueDestinationIds(formTemplate.destinations).toEither)
      _ <- fromOptA(DestinationsValidator.validateNoGroupInDeclaration(formTemplate.destinations).toEither)
      _ <- fromOptA(DestinationsValidator.validateDestinationIncludeIfs(formTemplate.destinations).toEither)
      _ <- fromOptA(
             DestinationsValidator
               .validateHandlebarSchemaCheck(formTemplate._id, formTemplate.destinations, handlebarsSchemaIds)
               .toEither
           )
      _ <- fromOptA(AcknowledgementValidator.validateNoPIITitle(formTemplate, allExpressions).toEither)
    } yield ()

  }

  def updateSection(section: Section): Section =
    section match {
      case p: Section.NonRepeatingPage => p.copy(page = mkSpecimen(p.page))
      case p: Section.RepeatingPage    => p.copy(page = mkSpecimen(p.page))
      case l: Section.AddToList =>
        l.copy(
          pages = l.pages.map(mkSpecimen),
          includeIf = None,
          fields = l.fields.flatMap(fields => removeIncludeIfFromFormComponents(fields.toList).toNel),
          repeatsUntil = None,
          repeatsWhile = None
        )
    }

  def mkSpecimen(formTemplate: FormTemplate): FormTemplate =
    formTemplate.copy(
      _id = FormTemplateId("specimen-" + formTemplate._id.value),
      authConfig = Anonymous,
      exitPages = None,
      formKind = formTemplate.formKind.fold[FormKind](classic =>
        classic.copy(
          sections = classic.sections.map(updateSection)
        )
      )(taskList =>
        taskList.copy(
          sections = taskList.sections.map(taskSection =>
            taskSection.copy(
              tasks = taskSection.tasks.map(task =>
                task.copy(
                  sections = task.sections.map(updateSection),
                  includeIf = None,
                  startIf = None,
                  summarySection = task.summarySection.map(summary => summary.copy(includeIf = None)),
                  declarationSection = task.declarationSection.map(declaration => declaration.copy(includeIf = None))
                )
              )
            )
          )
        )
      )
    )

  private def mkSpecimen(page: Page): Page =
    (removeIncludeIf _ andThen mkComponentsOptional)(page)

  private def removeIncludeIf(section: Page): Page =
    section.copy(includeIf = None, fields = removeIncludeIfFromFormComponents(section.fields))

  private def removeIncludeIfFromFormComponents(fcs: List[FormComponent]): List[FormComponent] = fcs.map {
    case fc @ IsChoice(choice) =>
      fc.copy(
        includeIf = None,
        `type` = choice.copy(
          options = choice.options.map(optionData => removeIncludeIfFromOptionData(optionData))
        )
      )
    case fc @ IsRevealingChoice(revealingChoice) =>
      fc.copy(
        includeIf = None,
        `type` = revealingChoice.copy(
          options = revealingChoice.options.map(rce => rce.copy(choice = removeIncludeIfFromOptionData(rce.choice)))
        )
      )
    case other => other.copy(includeIf = None)
  }

  private def removeIncludeIfFromOptionData(optionData: OptionData) =
    optionData match {
      case i @ OptionData.IndexBased(_, _, _, _)    => i.copy(includeIf = None)
      case i @ OptionData.ValueBased(_, _, _, _, _) => i.copy(includeIf = None)
    }

  private def mkComponentsOptional(page: Page): Page =
    page.copy(
      fields = mkOptional(page.fields)
    )

  private def mkOptional(fcs: List[FormComponent]): List[FormComponent] = fcs.map {
    case fc @ IsGroup(group) =>
      fc.copy(
        mandatory = false,
        `type` = group.copy(fields = mkOptional(group.fields))
      )
    case fc @ IsRevealingChoice(revealingChoice) =>
      fc.copy(
        mandatory = false,
        `type` = revealingChoice.copy(
          options = revealingChoice.options.map(rce => rce.copy(revealingFields = mkOptional(rce.revealingFields)))
        )
      )
    case fc => fc.copy(mandatory = false)
  }

}

object Verifier extends Verifier
