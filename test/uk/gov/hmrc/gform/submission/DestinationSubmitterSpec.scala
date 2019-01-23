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

import cats.{ Applicative, Monad }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.handlebarsHttpApiGen) {
      (si, handlebarsHttpApi) =>
        val model = HandlebarsTemplateProcessorModel(si.form)
        createSubmitter()
          .expectHandlebarsSubmission(handlebarsHttpApi, model, HttpResponse(200))
          .sut
          .apply(handlebarsHttpApi, si, model)
    }
  }

  "A Destination.DmsSubmission" should "be sent to the DmsSubmitter" in {
    forAll(DestinationSubmissionInfoGen.destinationSubmissionInfoGen, DestinationGen.hmrcDmsGen) {
      (si, hmrcDms: Destination.HmrcDms) =>
        val model = HandlebarsTemplateProcessorModel(si.form)
        createSubmitter()
          .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
          .sut
          .apply(hmrcDms, si, model)
    }
  }

  case class SubmitterParts[F[_]](
    sut: RealDestinationSubmitter[F],
    dmsSubmitter: DmsSubmitter[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter[F],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor) {
    def expectDmsSubmission(si: DestinationSubmissionInfo, dms: Destinations.DmsSubmission)(
      implicit F: Applicative[F]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, dms, hc)
        .returning(F.pure(()))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      model: HandlebarsTemplateProcessorModel,
      response: F[HttpResponse]): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(handlebarsHttpApi, model, hc)
        .returning(response)
      this
    }
  }

  private def createSubmitter[F[_]: Monad](): SubmitterParts[F] = {
    val dmsSubmitter = mock[DmsSubmitter[F]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[F]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val submitter = new RealDestinationSubmitter[F](dmsSubmitter, handlebarsSubmitter)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter, handlebarsTemplateProcessor)
  }
}
