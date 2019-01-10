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

import cats.data.NonEmptyList
import cats.{ Applicative, FlatMap }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationsSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "Destinations.DmsSubmission" should "be sent to the DmsSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationsGen.deprecatedDmsSubmissionGen) {
      (submissionInfo, dmsSubmission) =>
        val si = submissionInfo.copy(formTemplate = submissionInfo.formTemplate.copy(destinations = dmsSubmission))

        val sp = createSubmitter()
        import sp._

        (dmsSubmitter
          .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission))
          .expects(si, dmsSubmission)
          .returning(())

        submitter.send(si)
    }
  }

  "A Destination.HmrcDms" should "be sent to the DmsSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.hmrcDmsGen) {
      (submissionInfo, hmrcDms) =>
        val si = submissionInfo.copy(
          formTemplate =
            submissionInfo.formTemplate.copy(destinations = Destinations.DestinationList(NonEmptyList.of(hmrcDms))))

        val sp = createSubmitter()
        import sp._

        (dmsSubmitter
          .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission))
          .expects(si, hmrcDms.toDeprecatedDmsSubmission)
          .returning(())

        submitter.send(si)
    }
  }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.handlebarsHttpApiGen) {
      (submissionInfo, handlebarsHttpApi) =>
        val si = submissionInfo.copy(
          formTemplate = submissionInfo.formTemplate.copy(
            destinations = Destinations.DestinationList(NonEmptyList.of(handlebarsHttpApi))))

        val sp = createSubmitter()
        import sp._

        (handlebarsSubmitter
          .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
          .expects(handlebarsHttpApi, HandlebarsTemplateProcessorModel(si.form), hc)
          .returning(mock[HttpResponse])

        submitter.send(si)
    }
  }

  case class SubmitterParts[F[_]](
    submitter: DestinationsSubmitter[F],
    dmsSubmitter: DmsSubmitter[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter[F])

  private def createSubmitter[F[_]: FlatMap: Applicative](): SubmitterParts[F] = {
    val dmsSubmitter = mock[DmsSubmitter[F]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[F]]
    val submitter = new DestinationsSubmitter[F](dmsSubmitter, handlebarsSubmitter)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter)
  }
}
