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
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.destinations.DestinationsProcessorModelAlgebra
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationsSubmitterAlgebra[M[_]] {
  def send(submissionInfo: DestinationSubmissionInfo, modelTree: HandlebarsModelTree)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]]
}

class DestinationsSubmitter[M[_]](destinationSubmitter: DestinationSubmitter[M])(implicit monad: Monad[M])
    extends DestinationsSubmitterAlgebra[M] {

  override def send(submissionInfo: DestinationSubmissionInfo, modelTree: HandlebarsModelTree)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    modelTree.value.formTemplate.destinations match {
      case dms: Destinations.DmsSubmission =>
        destinationSubmitter
          .submitToDms(submissionInfo, modelTree.value.pdfData, modelTree.value.structuredFormData, dms)
          .map(_ => None)
      case list: Destinations.DestinationList =>
        submitToList(list, submissionInfo, HandlebarsTemplateProcessorModel.empty, modelTree)
    }

  def submitToList(
    destinations: Destinations.DestinationList,
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] = {
    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel)

    TailRecParameter(destinations.destinations.toList, accumulatedModel).tailRecM {
      case TailRecParameter(Nil, _) => Option.empty[HandlebarsDestinationResponse].asRight[TailRecParameter].pure[M]
      case TailRecParameter(head :: rest, updatedAccumulatedModel) =>
        destinationSubmitter
          .submitIfIncludeIf(head, submissionInfo, updatedAccumulatedModel, modelTree, this)
          .map(submitterResult =>
            TailRecParameter(rest, submitterResult.fold(updatedAccumulatedModel) {
              DestinationsProcessorModelAlgebra.createDestinationResponse(_) + updatedAccumulatedModel
            }).asLeft)
    }
  }
}
