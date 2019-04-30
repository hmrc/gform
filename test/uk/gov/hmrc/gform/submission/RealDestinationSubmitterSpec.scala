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

import cats.{ Applicative, Id, Monad, MonadError }
import cats.syntax.option._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormTemplateGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.generators.DestinationSubmissionInfoGen
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsHttpApiSubmitter, HandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.submission.ofsted.OfstedSubmitter
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class RealDestinationSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map {
      _.copy(formId = form._id)
    }

  "A Destination.ReviewApproval" should "be sent to the OfstedSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.reviewApprovalGen, FormTemplateGen.formTemplateGen) {
      (si, generatedDestination, template) =>
        val destination = generatedDestination.copy(includeIf = None)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectReviewApproval(si.formId, destination, model)
          .sut
          .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewApprovalGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewApproval(si.formId, destination, model)
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "not be sent to the OfstedSubmitter when includeIf is evaluated to false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewApprovalGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.ReviewRejection" should "be sent to the OfstedSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.reviewRejectionGen, FormTemplateGen.formTemplateGen) {
      (si, generatedDestination, template) =>
        val rejection = generatedDestination.copy(includeIf = None)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectReviewRejection(si.formId, rejection, model)
          .sut
          .submitIfIncludeIf(rejection, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewRejectionGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val rejection = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewRejection(si.formId, rejection, model)
        .sut
        .submitIfIncludeIf(rejection, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "not be sent to the OfstedSubmitter when includeIf is evaluated to false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewRejectionGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val rejection = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(rejection, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.ReviewingOfsed" should "be sent to the OfstedSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.reviewingOfstedGen, FormTemplateGen.formTemplateGen) {
      (si, generatedDestination, template) =>
        val destination = generatedDestination.copy(includeIf = None)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectReviewingOfstedSubmission(si, destination, model, FormId("foo"))
          .sut
          .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewingOfstedGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewingOfstedSubmission(si, destination, model, FormId("foo"))
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "not be sent to the OfstedSubmitter when includeIf is evaluated to false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewingOfstedGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHandlebarsHttpApi, template) =>
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = None)
        val httpResponse = HttpResponse(200)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
          .sut
          .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(
          HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedHandlebarsHttpApi, includeIfExpression, template) =>
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = Some(includeIfExpression))
      val httpResponse = HttpResponse(200)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(
        HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "not be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedHandlebarsHttpApi, includeIfExpression, template) =>
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.handlebarsHttpApiGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHandlebarsHttpApi, template) =>
        val httpResponse = HttpResponse(300)
        val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = Option(false), includeIf = None)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
          .sut
          .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(
          HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      Gen.option(Gen.const(true)),
      FormTemplateGen.formTemplateGen) { (si, generatedHandlebarsHttpApi, failOnError, template) =>
      val httpResponse = HttpResponse(300)
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = failOnError, includeIf = None)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Left(
        RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(handlebarsHttpApi, httpResponse))
    }
  }

  "A Destination.DmsSubmission" should "be sent to the DmsSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, template) =>
        val hmrcDms = generatedHmrcDms.copy(includeIf = None)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
          .sut
          .submitIfIncludeIf(hmrcDms, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.hmrcDmsGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedHmrcDms, includeIfExpression, template) =>
      val hmrcDms = generatedHmrcDms.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
        .sut
        .submitIfIncludeIf(hmrcDms, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.hmrcDmsGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedHmrcDms, includeIfExpression, template) =>
      val hmrcDms = generatedHmrcDms.copy(includeIf = Some(includeIfExpression))
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(hmrcDms, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is Some(false)" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, template) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = Option(false), includeIf = None)
        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .sut
          .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel(), submitter, template) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is Some(true) or None" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, Gen.option(Gen.const(true)), FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, failOnError, template) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = failOnError, includeIf = None)

        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .sut
          .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel(), submitter, template) shouldBe Left(
          s"Destination ${hmrcDms.id.id} : an error")
    }
  }

  case class SubmitterParts[F[_]](
    sut: RealDestinationSubmitter[F, Unit],
    dmsSubmitter: DmsSubmitter[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter[F],
    ofstedSubmitter: OfstedSubmitter[F],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor)(implicit F: MonadError[F, String]) {

    def expectDmsSubmission(si: DestinationSubmissionInfo, dms: Destinations.DmsSubmission)(
      implicit F: Applicative[F]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, dms, hc)
        .returning(F.pure(()))
      this
    }

    def expectDmsSubmissionFailure(
      si: DestinationSubmissionInfo,
      dms: Destinations.DmsSubmission,
      error: String): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, dms, hc)
        .returning(F.raiseError(error))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      model: HandlebarsTemplateProcessorModel,
      response: HttpResponse): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(handlebarsHttpApi, model, hc)
        .returning(F.pure(response))
      this
    }

    def expectReviewingOfstedSubmission(
      submissionInfo: DestinationSubmissionInfo,
      reviewingOfsted: Destination.ReviewingOfsted,
      model: HandlebarsTemplateProcessorModel,
      expectedReviewFormid: FormId): SubmitterParts[F] = {
      (ofstedSubmitter
        .submitForReview(_: DestinationSubmissionInfo, _: UserId, _: FormTemplateId, _: FormComponentId)(
          _: HeaderCarrier))
        .expects(
          submissionInfo,
          reviewingOfsted.userId,
          reviewingOfsted.reviewFormTemplateId,
          reviewingOfsted.correlationFieldId,
          hc)
        .returning(F.pure(expectedReviewFormid))
      this
    }

    def expectReviewRejection(
      formId: FormId,
      rejection: Destination.ReviewRejection,
      model: HandlebarsTemplateProcessorModel): SubmitterParts[F] = {
      (ofstedSubmitter
        .reject(_: FormId, _: FormComponentId, _: FormComponentId)(_: HeaderCarrier))
        .expects(formId, rejection.correlationFieldId, rejection.reviewFormCommentFieldId, hc)
        .returning(F.pure(()))
      this
    }

    def expectReviewApproval(
      formId: FormId,
      rejection: Destination.ReviewApproval,
      model: HandlebarsTemplateProcessorModel): SubmitterParts[F] = {
      (ofstedSubmitter
        .approve(_: FormId, _: FormComponentId, _: DestinationsSubmitter[F], _: FormTemplate)(_: HeaderCarrier))
        .expects(where {
          (formId: FormId, fcId: FormComponentId, _: DestinationsSubmitter[F], _: FormTemplate, hc: HeaderCarrier) =>
            formId === formId && fcId === rejection.correlationFieldId && hc === hc
        })
        .returning(F.pure(()))
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
    val ofsetReviewSubmitter = mock[OfstedSubmitter[Possible]]
    val submitter =
      new RealDestinationSubmitter[Possible, Unit](
        dmsSubmitter,
        handlebarsSubmitter,
        ofsetReviewSubmitter,
        handlebarsTemplateProcessor)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter, ofsetReviewSubmitter, handlebarsTemplateProcessor)
  }

  private def submitter(): DestinationsSubmitter[Possible] = {
    val destinationSubmitter: DestinationSubmitter[Possible] = mock[DestinationSubmitter[Possible]]
    new DestinationsSubmitter[Possible](destinationSubmitter)
  }
}
