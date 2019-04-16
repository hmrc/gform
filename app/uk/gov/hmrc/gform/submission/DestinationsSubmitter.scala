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

package uk.gov.hmrc.gform.submission

import cats.Monad
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.HeaderCarrier

class DestinationsSubmitter[M[_]](destinationSubmitter: DestinationSubmitter[M])(implicit monad: Monad[M]) {

  def send(submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    submissionInfo.formTemplate.destinations match {
      case dms: Destinations.DmsSubmission =>
        destinationSubmitter.submitToDms(submissionInfo, dms).map(_ => None)
      case list: Destinations.DestinationList => submitToList(list, submissionInfo)
    }

  private def submitToList(destinations: Destinations.DestinationList, submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    submitToList(
      destinations,
      submissionInfo,
      DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo))

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    handlebarsModel: HandlebarsTemplateProcessorModel)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] = {
    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel)

    TailRecParameter(destinations.destinations.toList, handlebarsModel).tailRecM {
      case TailRecParameter(Nil, _) => Option.empty[HandlebarsDestinationResponse].asRight[TailRecParameter].pure
      case TailRecParameter(head :: rest, model) =>
        destinationSubmitter
          .submitIfIncludeIf(head, submissionInfo, model)
          .map(submitterResult =>
            TailRecParameter(rest, submitterResult.fold(model) { HandlebarsTemplateProcessorModel(_) + model }).asLeft)
    }
  }
}

object DestinationsSubmitter {
  def createHandlebarsTemplateProcessorModel(
    submissionInfo: DestinationSubmissionInfo): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel.formId(submissionInfo.form) +
      HandlebarsTemplateProcessorModel(submissionInfo.structuredFormData) +
      HandlebarsTemplateProcessorModel.hmrcTaxPeriods(submissionInfo.form) +
      HandlebarsTemplateProcessorModel.rosmRegistration(submissionInfo.form) +
      HandlebarsTemplateProcessorModel(submissionInfo.variables)
}
