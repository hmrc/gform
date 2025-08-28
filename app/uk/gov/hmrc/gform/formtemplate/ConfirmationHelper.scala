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

import cats.Order
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Confirmation, Expr, FormComponentId, FormCtx, FormKind, FormTemplate, IsPostcodeLookup, Page, Section }

/* 1. Assuming that "importerUkAddress" is postcodeLookup component modify
 *
 * "confirmation": {
 *   "question": {
 *     ...
 *   },
 *   "fieldsConfirmed": [
 *     "importerUkAddress"
 *   ]
 * }
 *
 * to
 *
 * "confirmation": {
 *   "question": {
 *     ...
 *   },
 *   "expressionsConfirmed": [
 *     "${importerUkAddress}"
 *   ]
 * }
 *
 * Note. Due to complexity of postcodeLookup we cannot utilise standard component confirmation logic.
 *
 * 2. Assuming that "firstName" is text component modify
 *
 * "confirmation": {
 *   "question": {
 *     ...
 *   },
 *   "expressionsConfirmed": [
 *     "${firstName}"
 *   ]
 * }
 *
 * to
 *
 * "confirmation": {
 *   "question": {
 *     ...
 *   },
 *   "fieldsConfirmed": [
 *     "firstName"
 *   ]
 * }
 */
class ConfirmationHelper(formTemplate: FormTemplate) {

  val isPostcodeLookup: Set[FormComponentId] = SectionHelper
    .pages(formTemplate.formKind.allSections)
    .flatMap(_.fields)
    .collect { case fc @ IsPostcodeLookup(_) =>
      fc.id
    }
    .toSet

  def rewriteConfirmation(): FormTemplate = {
    val updateFormKind = formTemplate.formKind.fold[FormKind] { classic =>
      val sections = classic.sections.map(processSection)
      FormKind.Classic(sections)
    } { taskList =>
      taskList.copy(
        sections = taskList.sections.map { taskSection =>
          taskSection.copy(tasks = taskSection.tasks.map { task =>
            task.copy(sections = task.sections.map(processSection))
          })
        }
      )
    }
    formTemplate.copy(formKind = updateFormKind)
  }

  private def processSection(section: Section): Section =
    section.fold[Section](nonRepeatingPage => nonRepeatingPage.copy(page = processPage(nonRepeatingPage.page)))(
      repeatingPage => repeatingPage.copy(page = processPage(repeatingPage.page))
    )(addToList => addToList.copy(pages = addToList.pages.map(processPage)))

  private def processPage(page: Page): Page = {
    val confirmation = page.confirmation
      .map(confirmationFieldsAsExpr)
      .map(confirmationPostcodeLookup)
    page.copy(confirmation = confirmation)
  }

  private def confirmationPostcodeLookup(confirmation: Confirmation): Confirmation =
    confirmation.fieldsConfirmed match {
      case None => confirmation
      case Some(fieldsConfirmedAll) =>
        val (fieldsConfirmed, postcodeLookups) = groupPostcodeLookups(fieldsConfirmedAll)

        val expressionsConfirmed: Option[NonEmptyList[Expr]] = NonEmptyList.fromList(postcodeLookups) match {
          case None => confirmation.expressionsConfirmed
          case Some(postcodeLookupsNel) =>
            val newExprs: NonEmptyList[Expr] = postcodeLookupsNel.map(formComponentId => FormCtx(formComponentId))
            Some(confirmation.expressionsConfirmed.fold(newExprs) { exprs =>
              // Add postcodeLookup as expression to confirm only if it already doesn't exist
              NonEmptyList.fromList(newExprs.filter(expr => !exprs.toList.contains(expr))) match {
                case None                          => exprs
                case Some(uniquePostcodeLookupNel) => uniquePostcodeLookupNel.concatNel(exprs)
              }
            })
        }

        confirmation.copy(
          fieldsConfirmed = fieldsConfirmed,
          expressionsConfirmed = expressionsConfirmed
        )
    }

  private def confirmationFieldsAsExpr(confirmation: Confirmation): Confirmation =
    confirmation.expressionsConfirmed match {
      case None => confirmation
      case Some(exprsConfirmed) =>
        val fieldsFromExpressions: List[FormComponentId] = exprsConfirmed.toList.collect { case FormCtx(fcId) =>
          fcId
        }

        val fieldsConfirmed: Option[NonEmptyList[FormComponentId]] = confirmation.fieldsConfirmed
          .map(fieldsConfirmed => fieldsConfirmed.concat(fieldsFromExpressions))
          .orElse(NonEmptyList.fromList(fieldsFromExpressions))
          .map(_.distinct(Order.by((fcId: FormComponentId) => fcId.value)))

        val filteredExprs = exprsConfirmed.toList.filter {
          case FormCtx(_) => false
          case _          => true
        }

        val expressionsConfirmed = NonEmptyList.fromList(filteredExprs)

        confirmation.copy(
          fieldsConfirmed = fieldsConfirmed,
          expressionsConfirmed = expressionsConfirmed
        )
    }

  private def groupPostcodeLookups(
    fieldsConfirmed: NonEmptyList[FormComponentId]
  ): (Option[NonEmptyList[FormComponentId]], List[FormComponentId]) = {
    val postcodeLookups: List[FormComponentId] = fieldsConfirmed.filter(fcId => isPostcodeLookup(fcId))
    if (postcodeLookups.isEmpty) {
      (Some(fieldsConfirmed), List.empty)
    } else {
      (NonEmptyList.fromList(fieldsConfirmed.toList.filter(fcId => !postcodeLookups.contains(fcId))), postcodeLookups)
    }
  }
}

object ConfirmationHelper {
  def apply(formTemplate: FormTemplate): ConfirmationHelper = new ConfirmationHelper(formTemplate)
}
