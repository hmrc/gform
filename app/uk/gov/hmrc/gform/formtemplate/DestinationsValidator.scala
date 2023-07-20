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

import cats.implicits._
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.core.ValidationResult.BooleanToValidationResultSyntax
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.{ HandlebarValue, IncludeIfValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplate, IsGroup }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations }

object DestinationsValidator {
  def someDestinationIdsAreUsedMoreThanOnce(duplicates: Set[DestinationId]) =
    s"Some DestinationIds are defined more than once: ${duplicates.toList.sortBy(_.id).map(_.id)}"

  def groupComponentInDeclaration(groupComponentId: FormComponentId) =
    s"Group component $groupComponentId is not allowed in Declaration"

  def validateUniqueDestinationIds(destinations: Destinations): ValidationResult = destinations match {

    case _: Destinations.DestinationPrint => Valid

    case destinationList: Destinations.DestinationList =>
      val destinationIds = extractIds(destinationList.destinations)
      val duplicates = destinationIds.toList.groupBy(identity).collect { case (dId, List(_, _, _*)) => dId }.toSet
      duplicates.isEmpty.validationResult(someDestinationIdsAreUsedMoreThanOnce(duplicates))
  }

  def validateDestinationIncludeIfs(destinations: Destinations): ValidationResult = destinations match {
    case destinationList: Destinations.DestinationList =>
      val hasIncludeIfValue = destinationList.destinations.exists(_.includeIf match {
        case IncludeIfValue(_) => true
        case _                 => false
      })
      val hasHandlebarValue = destinationList.destinations.exists(_.includeIf match {
        case HandlebarValue(_) => true
        case _                 => false
      })

      if (hasIncludeIfValue && hasHandlebarValue) {
        Invalid(
          "IncludeIf statements in destinations are not valid. Destinations 'includeIf' must be either all 'gform expressions' ie. ${...} or all 'handlebar expressions' ie. {{...}}. It cannot be mix of both."
        )
      } else {
        Valid
      }

    case _ => Valid
  }

  def validateNoGroupInDeclaration(destinations: Destinations): ValidationResult = destinations match {
    case _: Destinations.DestinationPrint => Valid
    case destinationList: Destinations.DestinationList =>
      val groupComponentId = destinationList.declarationSection.flatMap(d => extractGroupComponentId(d.fields))
      groupComponentId match {
        case Some(id) => Invalid(groupComponentInDeclaration(id))
        case None     => Valid
      }
  }

  def validateDataStoreDestination(formTemplate: FormTemplate): ValidationResult = formTemplate.destinations match {
    case destinationList: Destinations.DestinationList =>
      destinationList.destinations.map {
        case _: Destination.DataStore if !formTemplate.isObjectStore =>
          Invalid("The destination type is invalid. 'hmrcIlluminate' can only be used for object-store.")
        case _ => Valid
      }.combineAll
    case _ => Valid
  }

  def extractGroupComponentId(fcs: List[FormComponent]): Option[FormComponentId] =
    fcs.collectFirst { case fc @ IsGroup(_) =>
      fc.id
    }

  private def extractIds(destinations: NonEmptyList[Destination]): NonEmptyList[DestinationId] =
    destinations.flatMap(extractIds)

  private def extractIds(destination: Destination): NonEmptyList[DestinationId] = destination match {
    case c: Destination.Composite => c.id :: extractIds(c.destinations)
    case _                        => NonEmptyList.of(destination.id)
  }
}
