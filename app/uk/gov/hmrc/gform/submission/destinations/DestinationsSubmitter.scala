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

package uk.gov.hmrc.gform.submission.destinations

import cats.{ Monad, MonadError }
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.bson.types.ObjectId
import play.api.Logging
import uk.gov.hmrc.gform.sdes.workitem.DestinationWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.sharedmodel.{ DestinationEvaluation, LangADT, UserSession }
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.http.HeaderCarrier

class DestinationsSubmitter[M[_]: Monad](
  destinationSubmitter: DestinationSubmitterAlgebra[M],
  workItemService: DestinationWorkItemAlgebra[M]
)(implicit
  monadError: MonadError[M, Throwable]
) extends DestinationsSubmitterAlgebra[M] with Logging {

  override def send(
    submissionInfo: DestinationSubmissionInfo,
    modelTree: HandlebarsModelTree,
    formData: Option[FormData],
    l: LangADT,
    destinationEvaluation: DestinationEvaluation,
    userSession: UserSession
  )(implicit hc: HeaderCarrier): M[Option[List[DestinationResponse]]] =
    modelTree.value.formTemplate.destinations match {
      case list: Destinations.DestinationList =>
        submitToList(
          list.destinations,
          submissionInfo,
          HandlebarsTemplateProcessorModel.empty,
          modelTree,
          formData,
          l,
          destinationEvaluation,
          userSession
        )

      case _ => Option.empty[List[DestinationResponse]].pure[M]
    }

  def submitToList(
    destinations: NonEmptyList[Destination],
    submissionInfo: DestinationSubmissionInfo,
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree,
    formData: Option[FormData],
    l: LangADT,
    destinationEvaluation: DestinationEvaluation,
    userSession: UserSession
  )(implicit hc: HeaderCarrier): M[Option[List[DestinationResponse]]] = {
    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel,
      accumulatedResponses: List[DestinationResponse]
    )

    TailRecParameter(destinations.toList, accumulatedModel, Nil).tailRecM {
      case TailRecParameter(Nil, _, responses) => Option(responses).asRight[TailRecParameter].pure[M]
      case TailRecParameter(head :: rest, updatedAccumulatedModel, updatedResponseList) =>
        val step: M[Either[TailRecParameter, Option[List[DestinationResponse]]]] = destinationSubmitter
          .submitIfIncludeIf(
            head,
            submissionInfo,
            updatedAccumulatedModel,
            modelTree,
            this,
            formData,
            l,
            destinationEvaluation,
            userSession
          )
          .map(submitterResult =>
            TailRecParameter(
              rest,
              submitterResult match {
                case h: HandlebarsDestinationResponse =>
                  DestinationsProcessorModelAlgebra.createDestinationResponse(h) + updatedAccumulatedModel
                case _ => updatedAccumulatedModel
              },
              submitterResult +: updatedResponseList
            ).asLeft
          )

        monadError.onError(step) { err =>
          logger.error(s"Critical error occurred during destination submission, cleaning up work items", err)
          cleanUp(updatedResponseList).void
        }
    }
  }

  private def cleanUp(forCleanup: List[DestinationResponse]): M[List[Unit]] = {
    def deleteWorkItem(workItemId: ObjectId, destination: SdesDestination): M[Unit] = {
      logger.info(s"Deleting deferred $destination work item ${workItemId.toHexString}")
      workItemService.delete(workItemId.toHexString, destination)
    }

    val data: List[(ObjectId, SdesDestination)] = forCleanup.collect {
      case d: DmsDestinationResponse       => (d.workItemId, d.routing)
      case o: OtherSdesDestinationResponse => (o.workItemId, o.routing)
    }

    data.traverse { case (workItemId, routing) =>
      deleteWorkItem(workItemId, routing)
    }
  }
}
