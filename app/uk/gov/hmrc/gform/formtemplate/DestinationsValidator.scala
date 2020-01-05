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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.core.ValidationResult.BooleanToValidationResultSyntax
import uk.gov.hmrc.gform.core.{ Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations }

import scala.collection.immutable.List

object DestinationsValidator {
  def someDestinationIdsAreUsedMoreThanOnce(duplicates: Set[DestinationId]) =
    s"Some DestinationIds are defined more than once: ${duplicates.toList.sortBy(_.id).map(_.id)}"

  def validateUniqueDestinationIds(destinations: Destinations): ValidationResult = destinations match {
    case _: Destinations.DmsSubmission => Valid

    case destinationList: Destinations.DestinationList =>
      val destinationIds = extractIds(destinationList.destinations)
      val duplicates = destinationIds.toList.groupBy(identity).collect { case (dId, List(_, _, _*)) => dId }.toSet
      duplicates.isEmpty.validationResult(someDestinationIdsAreUsedMoreThanOnce(duplicates))
  }

  private def extractIds(destinations: NonEmptyList[Destination]): NonEmptyList[DestinationId] =
    destinations.flatMap(extractIds)

  private def extractIds(destination: Destination): NonEmptyList[DestinationId] = destination match {
    case c: Destination.Composite => c.id :: extractIds(c.destinations)
    case _                        => NonEmptyList.of(destination.id)
  }

  def destinationTestReferencesANonExistentDestination(destinations: Set[DestinationId]): String =
    s"Destination tests refer to destinationIds that do not exist: ${destinations.map(_.id).mkString(", ")}"

  def validate(template: FormTemplate): Opt[Unit] =
    validateUniqueDestinationIds(template.destinations).toEither
}
