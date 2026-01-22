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

import cats.Monoid
import cats.implicits._
import cats.data.NonEmptyList
import uk.gov.hmrc.gform.core.ValidationResult.{ BooleanToValidationResultSyntax, validationResultMonoid }
import uk.gov.hmrc.gform.core.{ Invalid, Valid, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.HandlebarsSchemaId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.{ HandlebarValue, IncludeIfValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplateId, IsGroup }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.{ Dms, PegaCaseflow }

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

  def validateCaseflow(destinations: Destinations): ValidationResult = destinations match {
    case destinationList: Destinations.DestinationList =>
      val dmsList = destinationList.destinations.collect { case d: Destination.HmrcDms => d }

      val caseflows = dmsList.filter(dms => dms.routing === PegaCaseflow)
      val isDms = dmsList.exists(_.routing === Dms)

      val mixtureCheck = if (caseflows.nonEmpty && isDms) {
        Invalid(
          "hmrcDms destinations cannot be a mix of DMS and Pega Caseflow routings."
        )
      } else {
        Valid
      }

      val countCheck = if (caseflows.size > 1) {
        Invalid(
          "Cannot define more than 1 hmrcDms destination with routing to Pega Caseflow."
        )
      } else {
        Valid
      }

      val attributesCheck = caseflows.flatMap { dest =>
        List(
          if (dest.caseId.isEmpty) {
            Invalid(
              s"Pega Caseflow destination '${dest.id.id}' must have caseId expression defined."
            )
          } else {
            Valid
          },
          if (dest.postalCode.isEmpty) {
            Invalid(
              s"Pega Caseflow destination '${dest.id.id}' must have postalCode expression defined."
            )
          } else {
            Valid
          }
        )
      }

      Monoid[ValidationResult].combineAll(List(mixtureCheck, countCheck) ++ attributesCheck)
    case _ => Valid
  }

  def validateSubmissionPrefix(destinations: Destinations): ValidationResult = destinations match {
    case destinationList: Destinations.DestinationList =>
      val dmsList = destinationList.destinations.collect { case d: Destination.HmrcDms => d }

      val hasSubmissionPrefix = dmsList.exists(_.submissionPrefix.isDefined)
      val hasNoSubmissionPrefix = dmsList.exists(_.submissionPrefix.isEmpty)
      val allPrefixes = dmsList.map(_.submissionPrefix.getOrElse(""))

      val uniqueCheck = if (hasSubmissionPrefix && allPrefixes.size != allPrefixes.toSet.size) {
        Invalid(
          s"hmrcDms destinations must all have a unique submissionPrefix."
        )
      } else {
        Valid
      }

      val allOrNone = if (hasSubmissionPrefix && hasNoSubmissionPrefix) {
        Invalid(
          s"hmrcDms destinations must all have submissionPrefix or all not have submissionPrefix. It cannot be mix of both."
        )
      } else {
        Valid
      }

      Monoid[ValidationResult].combineAll(List(uniqueCheck, allOrNone))
    case _ => Valid
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

  def extractIds(destination: Destination): NonEmptyList[DestinationId] = NonEmptyList.of(destination.id)

  def validateRoboticsAsAttachment(destinations: Destinations): ValidationResult =
    destinations match {
      case Destinations.DestinationList(destinations, _, _) =>
        destinations.map {
          case destination: Destination.HmrcDms =>
            destination.roboticsAsAttachment -> destination.dataOutputFormat match {
              case (Some(true), None) =>
                Invalid(
                  s"""The destination '${destination.id.id}' is not valid. Once the property 'roboticsAsAttachment' is set to true, the 'dataOutputFormat' property must exist. Example: "dataOutputFormat": "xml" """
                )
              case _ => Valid
            }
          case _ => Valid
        }.combineAll
      case _ => Valid
    }
}
