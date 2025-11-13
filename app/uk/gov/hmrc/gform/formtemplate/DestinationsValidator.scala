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
import uk.gov.hmrc.gform.sharedmodel.HandlebarsSchemaId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.{ HandlebarValue, IncludeIfValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplateId, IsGroup }
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
      def validateIncludeIfValues(
        destinationType: String,
        destinations: NonEmptyList[Destination]
      ): ValidationResult = {
        val hasIncludeIfValue = destinations.exists(_.includeIf.isInstanceOf[IncludeIfValue])
        val hasHandlebarValue = destinations.exists(_.includeIf.isInstanceOf[HandlebarValue])

        if (hasIncludeIfValue && hasHandlebarValue) {
          Invalid(
            s"IncludeIf statements in $destinationType destinations are not valid. Destinations 'includeIf' must be either all 'gform expressions' ie. $${...} or all 'handlebar expressions' ie. {{...}}. It cannot be mix of both."
          )
        } else {
          Valid
        }
      }

      destinationList.destinations
        .groupBy {
          case _: Destination.HmrcDms                => Destination.hmrcDms
          case _: Destination.DataStore              => Destination.dataStore
          case _: Destination.InfoArchive            => Destination.infoArchive
          case _: Destination.HandlebarsHttpApi      => Destination.handlebarsHttpApi
          case _: Destination.Email                  => Destination.email
          case _: Destination.StateTransition        => Destination.stateTransition
          case _: Destination.SubmissionConsolidator => Destination.submissionConsolidator
          case _: Destination.Log                    => Destination.log
          case _: Destination.Composite              => Destination.composite
          case _: Destination.PegaApi                => Destination.pegaApi
          case _: Destination.NiRefundClaimApi       => Destination.niRefundClaimApi
        }
        .map { case (destinationType, destinations) => validateIncludeIfValues(destinationType, destinations) }
        .toList
        .combineAll

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

  def validateHandlebarSchemaCheck(
    formTemplateId: FormTemplateId,
    destinations: Destinations,
    handlebarsSchemaIds: List[HandlebarsSchemaId]
  ): ValidationResult =
    destinations match {
      case destinationList: Destinations.DestinationList =>
        destinationList.destinations.map {
          case ds: Destination.DataStore =>
            if (ds.validateHandlebarPayload && (ds.validateHandlebarPayload =!= ds.handlebarPayload))
              Invalid(
                s"The destination '${ds.id.id}' is not valid. Once the property 'validateHandlebarPayload' is set to true, the required property 'handlebarPayload' must be true."
              )
            else if (ds.validateHandlebarPayload && !handlebarsSchemaIds.map(_.value).contains(formTemplateId.value))
              Invalid(
                s"The destination '${ds.id.id}' is not valid. The schema '${formTemplateId.value}' does not exist."
              )
            else Valid
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

  def extractIds(destination: Destination): NonEmptyList[DestinationId] = destination match {
    case c: Destination.Composite => c.id :: extractIds(c.destinations)
    case _                        => NonEmptyList.of(destination.id)
  }
}
