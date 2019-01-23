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
import cats.syntax.functor._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.HeaderCarrier

trait DestinationSubmitter[M[_]] {
  def apply(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]]

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit]
}

class RealDestinationSubmitter[M[_]: Monad](dms: DmsSubmitter[M], handlebars: HandlebarsHttpApiSubmitter[M])
    extends DestinationSubmitter[M] {
  def apply(
    destination: Destination,
    submissionInfo: DestinationSubmissionInfo,
    model: HandlebarsTemplateProcessorModel)(implicit hc: HeaderCarrier): M[Option[HandlebarsTemplateProcessorModel]] =
    destination match {
      case d: Destination.HmrcDms => submitToDms(submissionInfo, d.toDeprecatedDmsSubmission).map(_ => None)
      case d: Destination.HandlebarsHttpApi =>
        handlebars(d, model).map(DestinationsSubmitter.createResponseModel(d, _)).map(Option(_))
    }

  def submitToDms(submissionInfo: DestinationSubmissionInfo, submission: Destinations.DmsSubmission)(
    implicit hc: HeaderCarrier): M[Unit] = dms(submissionInfo, submission)
}
