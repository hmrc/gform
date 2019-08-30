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

package uk.gov.hmrc.gform.submission.destinations

import cats.syntax.option._
import cats.{ Applicative, MonadError }
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.{ PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormTemplateGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.generators.{ PdfDataGen, StructuredFormValueGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsHttpApiSubmitter, HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationSubmitterSpec
    extends Spec with DestinationSubmissionInfoGen with DestinationGen with PrimitiveGen with FormTemplateGen
    with PdfDataGen with StructuredFormValueGen {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    destinationSubmissionInfoGen.map { dsi =>
      dsi.copy(submission = dsi.submission.copy(_id = form._id))
    }

  "A Destination.HandlebarsHttpApi" should "be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to true" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen,
      formTemplateGen,
      Gen.chooseNum(200, 299),
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, handlebarsHttpApi, template, responseCode, pdfData, structuredFormData) =>
      val httpResponse = HttpResponse(responseCode)
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectIncludeIfEvaluation(
          handlebarsHttpApi.includeIf,
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = true
        )
        .expectHandlebarsSubmission(handlebarsHttpApi, HandlebarsTemplateProcessorModel.empty, theTree, httpResponse)
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          None,
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, HandlebarsTemplateProcessorModel.empty, theTree, submitter) shouldBe Right(
        HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "not be sent to the HandlebarsHttpApiSubmitter when includeIf is evaluated to false" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen,
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, handlebarsHttpApi, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectIncludeIfEvaluation(
          handlebarsHttpApi.includeIf,
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = false
        )
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          tree(model, si.submission.submissionRef, template, pdfData, structuredFormData),
          submitter) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen,
      formTemplateGen,
      Gen.chooseNum(300, 500),
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, generatedHandlebarsHttpApi, template, responseCode, pdfData, structuredFormData) =>
      val httpResponse = HttpResponse(responseCode, responseString = Some("foo"))
      val handlebarsHttpApi = generatedHandlebarsHttpApi.copy(failOnError = false, includeIf = true.toString)
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, HandlebarsTemplateProcessorModel.empty, theTree, httpResponse)
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true)
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          None,
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model)
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          tree(model, si.submission.submissionRef, template, pdfData, structuredFormData),
          submitter) shouldBe Right(HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen(includeIf = Some(true.toString), failOnError = Some(true)),
      formTemplateGen,
      Gen.chooseNum(300, 500),
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, handlebarsHttpApi, template, responseCode, pdfData, structuredFormData) =>
      val httpResponse = HttpResponse(responseCode, responseString = Some("foobar"))
      val model = HandlebarsTemplateProcessorModel()

      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)
      createSubmitter
        .expectHandlebarsSubmission(handlebarsHttpApi, HandlebarsTemplateProcessorModel.empty, theTree, httpResponse)
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true)
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          Some("foobar"),
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model)
        .sut
        .submitIfIncludeIf(handlebarsHttpApi, si, HandlebarsTemplateProcessorModel.empty, theTree, submitter) shouldBe Left(
        genericLogMessage(
          si.formId,
          handlebarsHttpApi.id,
          DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(httpResponse)))
    }
  }

  "A Destination.DmsSubmission" should "be processed when includeIf is not set" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(includeIf = Some(true.toString)),
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen) { (si, hmrcDms, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectDmsSubmission(si, pdfData, structuredFormData, hmrcDms.toDeprecatedDmsSubmission)
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true)
        .expectDestinationAudit(hmrcDms, None, None, si.formId, pdfData, si.submission.submissionRef, template, model)
        .sut
        .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel.empty, theTree, submitter) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is true" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen,
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectIncludeIfEvaluation(
          hmrcDms.includeIf,
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = true
        )
        .expectDmsSubmission(si, pdfData, structuredFormData, hmrcDms.toDeprecatedDmsSubmission)
        .expectDestinationAudit(hmrcDms, None, None, si.formId, pdfData, si.submission.submissionRef, template, model)
        .sut
        .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel.empty, theTree, submitter) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is false" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen,
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectIncludeIfEvaluation(
          hmrcDms.includeIf,
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = false
        )
        .sut
        .submitIfIncludeIf(hmrcDms, si, model, theTree, submitter) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(includeIf = Some(true.toString), failOnError = Some(false)),
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen) { (si, hmrcDms, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectDmsSubmissionFailure(si, pdfData, structuredFormData, hmrcDms.toDeprecatedDmsSubmission, "an error")
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true
        )
        .expectDestinationAudit(hmrcDms, None, None, si.formId, pdfData, si.submission.submissionRef, template, model)
        .sut
        .submitIfIncludeIf(
          hmrcDms,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter
        ) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(failOnError = Some(true), includeIf = Some(true.toString)),
      formTemplateGen,
      pdfDataGen,
      structureFormValueObjectStructureGen) { (si, hmrcDms, template, pdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val theTree = tree(model, si.submission.submissionRef, template, pdfData, structuredFormData)

      createSubmitter
        .expectDmsSubmissionFailure(si, pdfData, structuredFormData, hmrcDms.toDeprecatedDmsSubmission, "an error")
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true
        )
        .sut
        .submitIfIncludeIf(hmrcDms, si, HandlebarsTemplateProcessorModel.empty, theTree, submitter) shouldBe Left(
        genericLogMessage(si.formId, hmrcDms.id, "an error"))
    }
  }

  case class SubmitterParts[F[_]](
    sut: DestinationSubmitter[F, Unit],
    dmsSubmitter: DmsSubmitterAlgebra[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter[F],
    destinationAuditer: DestinationAuditAlgebra[F],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor)(implicit F: MonadError[F, String]) {

    def expectDmsSubmission(
      si: DestinationSubmissionInfo,
      pdfData: PdfHtml,
      structuredFormData: StructuredFormValue.ObjectStructure,
      dms: Destinations.DmsSubmission)(implicit F: Applicative[F]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(
          _: DestinationSubmissionInfo,
          _: PdfHtml,
          _: StructuredFormValue.ObjectStructure,
          _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, pdfData, structuredFormData, dms, hc)
        .returning(F.pure(()))
      this
    }

    def expectDmsSubmissionFailure(
      si: DestinationSubmissionInfo,
      pdfData: PdfHtml,
      structuredFormData: StructuredFormValue.ObjectStructure,
      dms: Destinations.DmsSubmission,
      error: String): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(
          _: DestinationSubmissionInfo,
          _: PdfHtml,
          _: StructuredFormValue.ObjectStructure,
          _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(si, pdfData, structuredFormData, dms, hc)
        .returning(F.raiseError(error))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      tree: HandlebarsModelTree,
      response: HttpResponse): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(_: Destination.HandlebarsHttpApi, _: HandlebarsTemplateProcessorModel, _: HandlebarsModelTree)(
          _: HeaderCarrier))
        .expects(handlebarsHttpApi, accumulatedModel, tree, hc)
        .returning(F.pure(response))
      this
    }

    def expectIncludeIfEvaluation(
      expression: String,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      tree: FocussedHandlebarsModelTree,
      requiredResult: Boolean): SubmitterParts[F] = {
      (
        handlebarsTemplateProcessor
          .apply(
            _: String,
            _: HandlebarsTemplateProcessorModel,
            _: FocussedHandlebarsModelTree,
            _: TemplateType
          )
        )
        .expects(expression, accumulatedModel, tree, TemplateType.Plain)
        .returning(requiredResult.toString)
      this
    }

    def expectDestinationAudit(
      destination: Destination,
      responseCode: Option[Int],
      responseBody: Option[String],
      formId: FormId,
      pdfHtml: PdfHtml,
      submissionRef: SubmissionRef,
      template: FormTemplate,
      model: HandlebarsTemplateProcessorModel): SubmitterParts[F] = {
      (destinationAuditer
        .apply(
          _: Destination,
          _: Option[Int],
          _: Option[String],
          _: FormId,
          _: PdfHtml,
          _: SubmissionRef,
          _: FormTemplate,
          _: HandlebarsTemplateProcessorModel)(_: HeaderCarrier))
        .expects(destination, responseCode, responseBody, formId, pdfHtml, submissionRef, template, model, hc)
        .returning(F.pure(()))
      this
    }
  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val dmsSubmitter = mock[DmsSubmitterAlgebra[Possible]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter[Possible]]
    val stateTransitionService = mock[StateTransitionAlgebra[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val destinationAuditer = mock[DestinationAuditAlgebra[Possible]]
    val submitter =
      new DestinationSubmitter[Possible, Unit](
        dmsSubmitter,
        handlebarsSubmitter,
        stateTransitionService,
        Some(destinationAuditer),
        handlebarsTemplateProcessor)

    SubmitterParts(submitter, dmsSubmitter, handlebarsSubmitter, destinationAuditer, handlebarsTemplateProcessor)
  }

  private def submitter: DestinationsSubmitter[Possible] = {
    val destinationSubmitter: DestinationSubmitterAlgebra[Possible] = mock[DestinationSubmitterAlgebra[Possible]]
    new DestinationsSubmitter[Possible](destinationSubmitter)
  }

  def tree(
    model: HandlebarsTemplateProcessorModel,
    submissionRef: SubmissionRef,
    formTemplate: FormTemplate,
    pdfData: PdfHtml,
    structuredFormData: StructuredFormValue.ObjectStructure) =
    HandlebarsModelTree(submissionRef, formTemplate, pdfData, structuredFormData, model)
}
