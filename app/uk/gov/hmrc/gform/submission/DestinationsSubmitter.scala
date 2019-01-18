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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.HeaderCarrier

class DestinationsSubmitter[M[_]: Monad](
  dmsSubmitter: DmsSubmitter[M],
  handlebarsSubmitter: HandlebarsHttpApiSubmitter[M]) {

  private val monad: Monad[M] = implicitly[Monad[M]]

  def send(submissionInfo: DestinationSubmissionInfo)(implicit hc: HeaderCarrier): M[Unit] =
    submissionInfo.formTemplate.destinations match {
      case dms: Destinations.DmsSubmission    => submitToDms(submissionInfo, dms)
      case list: Destinations.DestinationList => submitToList(list, submissionInfo)
    }

  private def submitToDms(submissionInfo: DestinationSubmissionInfo, dms: Destinations.DmsSubmission): M[Unit] =
    dmsSubmitter(submissionInfo, dms)

  private def submitToList(destinations: Destinations.DestinationList, submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Unit] =
    monad.tailRecM[List[Destination], Unit](destinations.destinations.toList) {
      case Nil          => monad.pure(Right(()))
      case head :: rest => monad.map(submitToDestination(head, submissionInfo))(_ => Left(rest))
    }

  private def submitToDestination(destination: Destination, submissionInfo: DestinationSubmissionInfo)(
    implicit hc: HeaderCarrier): M[Unit] =
    destination match {
      case d: Destination.HmrcDms => submitToDms(submissionInfo, d.toDeprecatedDmsSubmission)
      case d: Destination.HandlebarsHttpApi =>
        monad.map(handlebarsSubmitter(d, createModel(submissionInfo)))(_ => ())
    }

  private def createModel(submissionInfo: DestinationSubmissionInfo): HandlebarsTemplateProcessorModel =
    HandlebarsTemplateProcessorModel(submissionInfo.form)
}
