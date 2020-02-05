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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.core.{ FOpt, fromOptA }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.implicits._

import scala.concurrent.ExecutionContext

trait Verifier {
  def verify(formTemplate: FormTemplate)(implicit ec: ExecutionContext): FOpt[Unit] = {

    val sections = formTemplate.sections

    val pages = SectionHelper.pages(sections)

    val componentTypes: List[ComponentType] = pages.flatMap(_.fields.map(_.`type`))

    val languages = formTemplate.languages

    for {
      _ <- fromOptA(FormTemplateValidator.validateLanguages(languages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateChoiceHelpText(pages).toEither)
      _ <- fromOptA(FormTemplateValidator.validateUniqueFields(sections).toEither)
      _ <- fromOptA(DestinationsValidator.validate(formTemplate))
      _ <- fromOptA(DestinationsValidator.validatePdfFieldIds(formTemplate))
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
      _ <- fromOptA(FormTemplateValidator.validateEmailVerification(formTemplate).toEither)
    } yield ()
  }

  def mkSpecimen(formTemplate: FormTemplate): FormTemplate =
    formTemplate.copy(
      _id = FormTemplateId("specimen-" + formTemplate._id.value),
      authConfig = Anonymous,
      sections = formTemplate.sections.flatMap {
        case p: Section.NonRepeatingPage => List(mkSpecimen(p.page))
        case p: Section.RepeatingPage    => List(mkSpecimen(p.page))
        case l: Section.AddToList        => l.pages.toList.map(mkSpecimen)
      }
    )

  private def mkSpecimen(page: Page): Section.NonRepeatingPage =
    Section.NonRepeatingPage(
      (removeIncludeIf _ andThen mkComponentsOptional _ andThen noValidators _)(page)
    )

  private def removeIncludeIf(section: Page): Page = section.copy(includeIf = None)

  private def mkComponentsOptional(page: Page): Page =
    page.copy(
      fields = mkOptional(page.fields)
    )

  private def noValidators(section: Page): Page =
    section.copy(validators = None)

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
          options = revealingChoice.options.map(rce => rce.copy(revealingFields = mkOptional(rce.revealingFields))))
      )
    case fc => fc.copy(mandatory = false)
  }

}
