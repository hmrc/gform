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
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ DestinationSubmissionInfo, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
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

  "A Destination.ReviewApproval" should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewApprovalGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewApproval(si.formId, destination, model)
        .expectDestinationAudit(destination.id, None, si.formId)
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
      val destination = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.ReviewRejection" should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewRejectionGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val rejection = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewRejection(si.formId, rejection, model)
        .expectDestinationAudit(rejection.id, None, si.formId)
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
      val rejection = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(rejection, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.ReviewingOfsed" should "be sent to the OfstedSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.reviewingOfstedGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen) { (si, generatedDestination, includeIfExpression, template) =>
      val destination = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectReviewingOfstedSubmission(si, destination, model, FormId("foo"))
        .expectDestinationAudit(destination.id, None, si.formId)
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
      val destination = generatedDestination.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(destination, si, model, submitter, template) shouldBe Right(None)
    }
  }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      PrimitiveGen.nonEmptyAlphaNumStrGen,
      FormTemplateGen.formTemplateGen,
      Gen.chooseNum(200, 299)
    ) { (si, generatedHandlebarsHttpApi, includeIfExpression, template, responseCode) =>
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = includeIfExpression)
      val httpResponse = HttpResponse(responseCode)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .expectDestinationAudit(handlebarsHttpApi.id, Some(responseCode), si.formId)
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
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      FormTemplateGen.formTemplateGen,
      Gen.chooseNum(300, 500)) { (si, generatedHandlebarsHttpApi, template, responseCode) =>
      val httpResponse = HttpResponse(responseCode)
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = false, includeIf = true.toString)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .expectIncludeIfEvaluation("true", model, true)
        .expectDestinationAudit(handlebarsHttpApi.id, Some(responseCode), si.formId)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Right(
        HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      FormTemplateGen.formTemplateGen,
      Gen.chooseNum(300, 500)) { (si, generatedHandlebarsHttpApi, template, responseCode) =>
      val httpResponse = HttpResponse(responseCode)
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = true, includeIf = true.toString)
      val model = HandlebarsTemplateProcessorModel()

      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, model, httpResponse)
        .expectIncludeIfEvaluation("true", model, true)
        .expectDestinationAudit(handlebarsHttpApi.id, Some(responseCode), si.formId)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, model, submitter, template) shouldBe Left(
        RealDestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(handlebarsHttpApi, httpResponse))
    }
  }

  "A Destination.DmsSubmission" should "be sent to the DmsSubmitter when includeIf is not set" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, template) =>
        val hmrcDms = generatedHmrcDms.copy(includeIf = true.toString)
        val model = HandlebarsTemplateProcessorModel()
        createSubmitter
          .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
          .expectIncludeIfEvaluation("true", model, true)
          .expectDestinationAudit(hmrcDms.id, None, si.formId)
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
      val hmrcDms = generatedHmrcDms.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = true)
        .expectDmsSubmission(si, hmrcDms.toDeprecatedDmsSubmission)
        .expectDestinationAudit(hmrcDms.id, None, si.formId)
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
      val hmrcDms = generatedHmrcDms.copy(includeIf = includeIfExpression)
      val model = HandlebarsTemplateProcessorModel()
      createSubmitter
        .expectIncludeIfEvaluation(includeIfExpression, model, requiredResult = false)
        .sut
        .submitIfIncludeIf(hmrcDms, si, model, submitter, template) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, template) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = false, includeIf = true.toString)
        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .expectIncludeIfEvaluation("true", HandlebarsTemplateProcessorModel.empty, true)
          .expectDestinationAudit(hmrcDms.id, None, si.formId)
          .sut
          .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel(), submitter, template) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is true" in {
    forAll(submissionInfoGen, DestinationGen.hmrcDmsGen, FormTemplateGen.formTemplateGen) {
      (si, generatedHmrcDms, template) =>
        val hmrcDms = generatedHmrcDms.copy(failOnError = true, includeIf = true.toString)

        createSubmitter
          .expectDmsSubmissionFailure(si, hmrcDms.toDeprecatedDmsSubmission, "an error")
          .expectIncludeIfEvaluation("true", HandlebarsTemplateProcessorModel.empty, true)
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
    destinationAuditer: DestinationAuditAlgebra[F],
    formAlgebra: FormAlgebra[F],
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

    def expectDestinationAudit(
      destinationId: DestinationId,
      response: Option[Int],
      formId: FormId): SubmitterParts[F] = {
      (destinationAuditer
        .apply(_: DestinationId, _: Option[Int], _: FormId)(_: HeaderCarrier))
        .expects(destinationId, response, formId, hc)
        .returning(F.pure(()))
      this
    }
  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val dmsSubmitter = mock[DmsSubmitter[Possible]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val ofsetReviewSubmitter = mock[OfstedSubmitter[Possible]]
    val destinationAuditer = mock[DestinationAuditAlgebra[Possible]]
    val formAlgebra = mock[FormAlgebra[Possible]]
    val submitter =
      new RealDestinationSubmitter[Possible, Unit](
        dmsSubmitter,
        handlebarsSubmitter,
        ofsetReviewSubmitter,
        destinationAuditer,
        formAlgebra,
        handlebarsTemplateProcessor)

    SubmitterParts(
      submitter,
      dmsSubmitter,
      handlebarsSubmitter,
      ofsetReviewSubmitter,
      destinationAuditer,
      formAlgebra,
      handlebarsTemplateProcessor)
  }

  private def submitter: DestinationsSubmitter[Possible] = {
    val destinationSubmitter: DestinationSubmitter[Possible] = mock[DestinationSubmitter[Possible]]
    new DestinationsSubmitter[Possible](destinationSubmitter)
  }
}
