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
import cats.syntax.option._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, PrimitiveGen }
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class RealDestinationSubmitterSpec extends Spec with ExampleData {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map {
      _.copy(form = form, formTemplate = formTemplate)
    }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen) { (si, generatedHandlebarsHttpApi) =>
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = None)
      val httpResponse = HttpResponse(200)
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model) shouldBe Right(
        HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to true" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, PrimitiveGen.nonEmptyAlphaNumStrGen) {
      (si, generatedHandlebarsHttpApi, includeIfExpression) =>
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = Some(includeIfExpression))
        val httpResponse = HttpResponse(200)
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)

        createSubmitter
          .expectIncludeIfEvaluation(includeIfExpression, model, true)
          .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
          .sut
          .submitIfIncludeIf(handlebarsHttpApi, si, model) shouldBe Right(
          HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "not be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to false" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, PrimitiveGen.nonEmptyAlphaNumStrGen) {
      (si, generatedHandlebarsHttpApi, includeIfExpression) =>
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = Some(includeIfExpression))
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
        createSubmitter
          .expectIncludeIfEvaluation(includeIfExpression, model, false)
          .sut
          .submitIfIncludeIf(handlebarsHttpApi, si, model) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen) { (si, generatedHandlebarsHttpApi) =>
      val httpResponse = HttpResponse(300)
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = Option(false), includeIf = None)
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model) shouldBe Right(
        HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, Gen.option(Gen.const(true))) {
      (si, generatedHandlebarsHttpApi, failOnError) =>
        val httpResponse = HttpResponse(300)
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = failOnError, includeIf = None)
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)

        createSubmitter
          .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
          .sut
          .submitIfIncludeIf(handlebarsHttpApi, si, model) shouldBe Left(
          RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(handlebarsHttpApi, httpResponse))
    }
  }

  "A Destination.DmsSubmission" should "be sent to the DmsSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen) { (si, generatedHmrcDms) =>
      val hmrcDms = generatedHmrcDms.copy(includeIf = None)
      val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
      createSubmitter
        .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
        .sut
        .submitIfIncludeIf(hmrcDms, si, model) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is true" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, PrimitiveGen.nonEmptyAlphaNumStrGen) {
      (si, generatedHmrcDms, includeIfExpression) =>
        val hmrcDms = generatedHmrcDms.copy(includeIf = Some(includeIfExpression))
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
        createSubmitter
          .expectIncludeIfEvaluation(includeIfExpression, model, true)
          .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
          .sut
          .submitIfIncludeIf(hmrcDms, si, model) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is false" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, PrimitiveGen.nonEmptyAlphaNumStrGen) {
      (si, generatedHmrcDms, includeIfExpression) =>
        val hmrcDms = generatedHmrcDms.copy(includeIf = Some(includeIfExpression))
        val model = HandlebarsTemplateProcessorModel(si.form, si.formTemplate)
        createSubmitter
          .expectIncludeIfEvaluation(includeIfExpression, model, false)
          .sut
          .submitIfIncludeIf(hmrcDms, si, model) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen) { (si, generatedHmrcDms) =>
      val hmrcDms = generatedHmrcDms.copy(failOnError = Option(false), includeIf = None)
      createSubmitter
        .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
        .sut
        .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel(si.form, si.formTemplate)) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, Gen.option(Gen.const(true))) {
      (si, generatedHmrcDms, failOnError) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = failOnError, includeIf = None)

        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .sut
          .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel(si.form, si.formTemplate)) shouldBe Left(
          s"Destination ${hmrcDms.id.id} : an error")
    }
  }

  case class SubmitterParts[F[_]](
    sut: RealDestinationSubmitter[F, Unit],
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

    def expectIncludeIfEvaluation(
      expression: String,
      model: HandlebarsTemplateProcessorModel,
      requiredResult: Boolean): SubmitterParts[F] = {
      (handlebarsTemplateProcessor
        .apply(_: String, _: HandlebarsTemplateProcessorModel))
        .expects(expression, model)
        .returning(requiredResult.toString)
      this
    }

  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val dmsSubmitter = mock[DmsSubmitter[Possible]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val submitter =
      new RealDestinationSubmitter[Possible, Unit](dmsSubmitter, handlebarsSubmitter, handlebarsTemplateProcessor)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter, handlebarsTemplateProcessor)
  }
}
