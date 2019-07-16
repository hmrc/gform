/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.Possible
import uk.gov.hmrc.gform.core.ValidationResult.BooleanToValidationResultSyntax
import uk.gov.hmrc.gform.core.{ Opt, Valid, ValidationResult }
import uk.gov.hmrc.gform.fileupload.FileDownloadAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationId, DestinationTest, Destinations }
import uk.gov.hmrc.gform.submission.{ DestinationSubmissionInfo, DestinationsSubmitter, SelfTestingDestinationSubmitter, SubmissionRef }
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.immutable.List

object DestinationsValidator {
  def someDestinationIdsAreUsedMoreThanOnce(duplicates: Set[DestinationId]) =
    s"Some DestinationIds are defined more than once: ${duplicates.toList.sortBy(_.id).map(_.id)}"

  def validateUniqueDestinationIds(destinations: Destinations): ValidationResult = destinations match {
    case _: Destinations.DmsSubmission => Valid

    case destinationList: Destinations.DestinationList =>
      val destinationIds = destinationList.destinations.map(_.id)
      val duplicates = destinationIds.toList.groupBy(identity).collect { case (dId, List(_, _, _*)) => dId }.toSet
      duplicates.isEmpty.validationResult(someDestinationIdsAreUsedMoreThanOnce(duplicates))
  }

  def destinationTestReferencesANonExistentDestination(destinations: Set[DestinationId]): String =
    s"Destination tests refer to destinationIds that do not exist: ${destinations.map(_.id).mkString(", ")}"

  def validateTestDestinationIdsExist(destinations: Destinations, tests: List[DestinationTest]): ValidationResult =
    destinations match {
      case destinationList: Destinations.DestinationList =>
        validateDestinationListTestReferences(destinationList, tests)
      case _: Destinations.DmsSubmission => Valid
    }

  private def validateDestinationListTestReferences(
    destinations: Destinations.DestinationList,
    tests: List[DestinationTest]): ValidationResult = {
    val destinationIds = destinations.destinations.map(_.id).toList.toSet
    val referencedDestinationIds = tests.flatMap(_.expectedResults.map(_.destinationId)).toSet

    val badDestinationIds = referencedDestinationIds -- destinationIds
    badDestinationIds.isEmpty.validationResult(destinationTestReferencesANonExistentDestination(badDestinationIds))
  }

  def validateTests(template: FormTemplate): ValidationResult =
    template.destinations match {
      case destinationList: Destinations.DestinationList => validateTests(template, destinationList)
      case _: Destinations.DmsSubmission                 => Valid
    }

  private def validateTests(template: FormTemplate, dl: Destinations.DestinationList): ValidationResult = {
    val errors = template.destinationTests.toList.flatten
      .map { t =>
        val submitter = new DestinationsSubmitter[Possible](
          new SelfTestingDestinationSubmitter[Possible](test = t),
          Option.empty[FileDownloadAlgebra[Possible]])
        submitter
          .submitToList(dl, DestinationSubmissionInfo(null, null, None, null, SubmissionRef("Test")), t.formData, null)(
            HeaderCarrier())
      }
      .collect { case Left(error) => error }

    errors.isEmpty.validationResult(errors.mkString(", "))
  }

  def validate(template: FormTemplate): Opt[Unit] =
    for {
      _ <- validateUniqueDestinationIds(template.destinations).toEither
      _ <- validateTestDestinationIdsExist(template.destinations, template.destinationTests.toList.flatten).toEither
      _ <- validateTests(template).toEither
    } yield ()
}
