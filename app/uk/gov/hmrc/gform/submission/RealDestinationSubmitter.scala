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

import cats.MonadError
import cats.syntax.functor._
import cats.syntax.flatMap._
import play.api.Logger
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.wshttp.HttpResponseSyntax

trait DestinationSubmitter[M[_]] {
  def apply(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]]

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit]
}

class RealDestinationSubmitter[M[_]](dms: DmsSubmitter[M], handlebars: HandlebarsHttpApiSubmitter[M])(
  implicit monadError: MonadError[M, String])
    extends DestinationSubmitter[M] {
  def apply(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]] =
    destination match {
      case d: Destination.HmrcDms           => submitToDms(submissionInfo, d).map(_ => None)
      case d: Destination.HandlebarsHttpApi => submitToHandlebars(d, model)
    }

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = dms(submissionInfo, submission)

  private def submitToDms(submissionInfo: DestinationSubmissionInfo, d: Destination.HmrcDms)(
    implicit hc: HeaderCarrier): M[Unit] =
    monadError.handleErrorWith(submitToDms(submissionInfo, d.toDeprecatedDmsSubmission)) { msg =>
      if (d.failOnErrorDefaulted)
        monadError.raiseError(msg)
      else {
        Logger.info(s"Destination ${d.id} failed but has 'failOnError' set to false. Ignoring.")
        monadError.pure(())
      }
    }

  private def submitToHandlebars(d: Destination.HandlebarsHttpApi, model: HandlebarsTemplateProcessorModel)(
    implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]] =
    handlebars(d, model)
      .flatMap[HandlebarsTemplateProcessorModel] { response =>
        if (response.isSuccess)
          createSuccessResponse(d, response)
        else if (d.failOnErrorDefaulted)
          createFailureResponse(d, response)
        else {
          Logger.info(
            s"Destination ${d.id} returned status code ${response.status} but has 'failOnError' set to false. Ignoring.")
          createSuccessResponse(d, response)
        }
      }
      .map(Option(_))

  private def createFailureResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsTemplateProcessorModel] =
    monadError.raiseError(DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(d, response))

  private def createSuccessResponse(
    d: Destination.HandlebarsHttpApi,
    response: HttpResponse): M[HandlebarsTemplateProcessorModel] =
    monadError.pure(DestinationsSubmitter.createResponseModel(d, response))
}

object DestinationSubmitter {
  def handlebarsHttpApiFailOnErrorMessage(d: Destination.HandlebarsHttpApi, response: HttpResponse): String =
    s"Destination ${d.id} returned status code ${response.status} and has 'failOnError' set to true. Failing."
}
