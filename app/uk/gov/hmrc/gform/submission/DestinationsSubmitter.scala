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
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsTemplateProcessor, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationsSubmitter[M[_]: Monad](
  destinationSubmitter: DestinationSubmitter[M],
  handlebarsTemplateProcessor: HandlebarsTemplateProcessor = new HandlebarsTemplateProcessor) {

  def send(submissionInfo: DestinationSubmissionInfo)(implicit hc: HeaderCarrier): M[Unit] =
    submissionInfo.formTemplate.destinations match {
      case dms: Destinations.DmsSubmission    => destinationSubmitter.submitToDms(submissionInfo, dms)
      case list: Destinations.DestinationList => submitToList(list, submissionInfo)
    }

  private def submitToList(destinations: Destinations.DestinationList, submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Unit] = {

    case class TailRecParameter(
      remainingDestinations: List[Destination],
      accumulatedModel: HandlebarsTemplateProcessorModel)

    TailRecParameter(
      destinations.destinations.toList,
      DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo)).tailRecM {
      case TailRecParameter(Nil, _) => ().asRight[TailRecParameter].pure[M]
      case TailRecParameter(head :: rest, model) =>
        maybeSubmitToDestination(head, submissionInfo, model).map(responseModel =>
          TailRecParameter(rest, responseModel.fold(model) { _ + model }).asLeft)
    }
  }

  private def maybeSubmitToDestination(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]] =
    if (destination.includeIf.forall(handlebarsTemplateProcessor(_, model) === true.toString))
      destinationSubmitter(destination, submissionInfo, model)
    else Option.empty[HandlebarsTemplateProcessorModel].pure
}

object DestinationsSubmitter {
  def createResponseModel(
    destination: Destination.HandlebarsHttpApi,
    response: HttpResponse): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(s"""|{
                                         |  "${destination.id.id}" : {
                                         |    "status" : ${response.status},
                                         |    "json" : ${response.json}
                                         |  }
                                         |}""".stripMargin)

  def createHandlebarsTemplateProcessorModel(
    submissionInfo: DestinationSubmissionInfo): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(submissionInfo.form)
}
