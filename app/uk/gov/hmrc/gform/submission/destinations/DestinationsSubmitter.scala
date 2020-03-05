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

package uk.gov.hmrc.gform.submission.destinations

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.http.HeaderCarrier

class DestinationsSubmitter[M[_]: Monad](destinationSubmitter: DestinationSubmitterAlgebra[M])
    extends DestinationsSubmitterAlgebra[M] {

  override def send(submissionInfo: DestinationSubmissionInfo, modelTree: HandlebarsModelTree)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] =
    modelTree.value.formTemplate.destinations match {
      case list: Destinations.DestinationList =>
        submitToList(list.destinations, submissionInfo, HandlebarsTemplateProcessorModel.empty, modelTree)

      case _ => Option.empty[HandlebarsDestinationResponse].pure[M]
    }

  def submitToList(
    destinations: NonEmptyList[Destination],
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)(implicit hc: HeaderCarrier): M[Option[HandlebarsDestinationResponse]] = {
    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel)

    TailRecParameter(destinations.toList, accumulatedModel).tailRecM {
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
