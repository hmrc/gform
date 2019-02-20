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

import cats.{ Applicative, MonadError }
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationSubmitterSpec extends Spec with ExampleData {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map {
      _.copy(form = form, formTemplate = formTemplate)
    }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen) { (si, handlebarsHttpApi) =>
      val httpResponse = HttpResponse(200)
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .apply(handlebarsHttpApi, si, model) shouldBe Right(
        Option(DestinationsSubmitter.createResponseModel(handlebarsHttpApi, httpResponse)))
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen) { (si, generatedHandlebarsHttpApi) =>
      val httpResponse = HttpResponse(300)
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = Option(false))
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .apply(handlebarsHttpApi, si, model) shouldBe Right(
        Option(DestinationsSubmitter.createResponseModel(handlebarsHttpApi, httpResponse)))
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, Gen.option(Gen.const(true))) {
      (si, generatedHandlebarsHttpApi, failOnError) =>
        val httpResponse = HttpResponse(300)
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = failOnError)
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)

        createSubmitter
          .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
          .sut
          .apply(handlebarsHttpApi, si, model) shouldBe Left(
          DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(handlebarsHttpApi, httpResponse))
    }
  }

  "A Destination.DmsSubmission" should "be sent to the DmsSubmitter" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen) { (si, hmrcDms) =>
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
        .sut
        .apply(hmrcDms, si, model) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen) { (si, generatedHmrcDms) =>
      val hmrcDms = generatedHmrcDms.copy(failOnError = Option(false))
      createSubmitter
        .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
        .sut
        .apply(hmrcDms, si, HandlebarsTemplateProcessorModel(si.form, si.formTemplate)) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, Gen.option(Gen.const(true))) {
      (si, generatedHmrcDms, failOnError) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = failOnError)

        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .sut
          .apply(hmrcDms, si, HandlebarsTemplateProcessorModel(si.form, si.formTemplate)) shouldBe Left("an error")
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

    def expectDmsSubmissionFailure(si: DestinationSubmissionInfo, dms: Destinations.DmsSubmission, error: String)(
      implicit F: MonadError[F, String]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, dms, hc)
        .returning(F.raiseError(error))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      model: HandlebarsTemplateProcessorModel,
      response: HttpResponse)(implicit F: Applicative[F]): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(handlebarsHttpApi, model, hc)
        .returning(F.pure(response))
      this
    }
  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val dmsSubmitter = mock[DmsSubmitter[Possible]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val submitter = new RealDestinationSubmitter[Possible](dmsSubmitter, handlebarsSubmitter)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter, handlebarsTemplateProcessor)
  }
}
