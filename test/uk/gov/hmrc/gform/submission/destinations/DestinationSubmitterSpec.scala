/*
 * Copyright 2023 HM Revenue & Customs
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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.gform.notifier.NotifierAlgebra
import uk.gov.hmrc.gform.sdes.{ SdesConfig, SdesRouting, WelshDefaults }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.{ HmrcDms, InfoArchive, SubmissionConsolidator }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, FormTemplateGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.generators.{ PdfDataGen, StructuredFormValueGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsHttpApiSubmitter, HandlebarsModelTree, HandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.submissionconsolidator.SubmissionConsolidatorAlgebra
import uk.gov.hmrc.gform.{ Possible, Spec, possibleMonadError }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.DurationInt
import scala.util.{ Failure, Success, Try }

class DestinationSubmitterSpec
    extends Spec with DestinationSubmissionInfoGen with DestinationGen with PrimitiveGen with FormTemplateGen
    with PdfDataGen with StructuredFormValueGen with ScalaCheckDrivenPropertyChecks {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  implicit val testFutureConverter: FutureConverter[({ type L[A] = Either[Throwable, A] })#L] =
    new FutureConverter[({ type L[A] = Either[Throwable, A] })#L] {
      def fromFuture[A](future: Future[A]): Either[Throwable, A] =
        Try(Await.result(future, 10.seconds)) match {
          case Success(value)     => Right(value)
          case Failure(throwable) => Left(throwable)
        }
    }

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    destinationSubmissionInfoGen.map { dsi =>
      dsi.copy(submission = dsi.submission.copy(_id = dsi.submission._id.copy(formId = form._id)))
    }

  def getHandlebarValue(includeIf: DestinationIncludeIf): String =
    includeIf match {
      case HandlebarValue(s) => s
      case _                 => ""
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
      val httpResponse = HttpResponse(responseCode, null)
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(
        si.formId,
        model,
        si.submission.submissionRef,
        template,
        pdfData,
        None,
        structuredFormData
      )

      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(handlebarsHttpApi.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = true
        )
        .expectHandlebarsSubmission(
          handlebarsHttpApi,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          httpResponse,
          si
        )
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          None,
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model
        )
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
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
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          None,
          structuredFormData
        )

      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(handlebarsHttpApi.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = false
        )
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(None)
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
      val httpResponse = HttpResponse(responseCode, "foo")
      val handlebarsHttpApi =
        generatedHandlebarsHttpApi.copy(failOnError = false, includeIf = HandlebarValue(true.toString))
      val model = HandlebarsTemplateProcessorModel()
      val theTree = tree(
        si.formId,
        model,
        si.submission.submissionRef,
        template,
        pdfData,
        None,
        structuredFormData
      )

      createSubmitter
        .expectHandlebarsSubmission(
          handlebarsHttpApi,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          httpResponse,
          si
        )
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true
        )
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          None,
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model
        )
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(HandlebarsDestinationResponse(handlebarsHttpApi, httpResponse).some)
    }
  }

  it should "raise an error if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen(includeIf = Some(HandlebarValue(true.toString)), failOnError = Some(true)),
      formTemplateGen,
      Gen.chooseNum(300, 500),
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, handlebarsHttpApi, template, responseCode, pdfData, structuredFormData) =>
      val httpResponse = HttpResponse(responseCode, "foobar")
      val model = HandlebarsTemplateProcessorModel()

      val theTree = tree(
        si.formId,
        model,
        si.submission.submissionRef,
        template,
        pdfData,
        None,
        structuredFormData
      )
      val result: Possible[Option[HandlebarsDestinationResponse]] = createSubmitter
        .expectHandlebarsSubmission(
          handlebarsHttpApi,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          httpResponse,
          si
        )
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true
        )
        .expectDestinationAudit(
          handlebarsHttpApi,
          Some(responseCode),
          Some("foobar"),
          si.formId,
          pdfData,
          si.submission.submissionRef,
          template,
          model
        )
        .sut
        .submitIfIncludeIf(
          handlebarsHttpApi,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        )

      result match {
        case Left(throwable) =>
          val expectedMessage = genericLogMessage(
            si.formId,
            handlebarsHttpApi.id,
            DestinationSubmitter.handlebarsHttpApiFailOnErrorMessage(httpResponse)
          )
          throwable.getMessage shouldBe expectedMessage
        case Right(_) => fail("Expected handlebar error")
      }
    }
  }

  "A Destination.DmsSubmission" should "be processed when includeIf is not set" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(includeIf = Some(HandlebarValue(true.toString))),
      formTemplateGen,
      pdfDataGen,
      instructionPdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, instructionPdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          instructionPdfData,
          structuredFormData
        )

      createSubmitter
        .expectHmrcDmsSubmission(si, HandlebarsTemplateProcessorModel.empty, theTree, hmrcDms, LangADT.En)
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
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is true" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen,
      formTemplateGen,
      pdfDataGen,
      instructionPdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, instructionPdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          instructionPdfData,
          structuredFormData
        )

      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(hmrcDms.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = true
        )
        .expectHmrcDmsSubmission(si, HandlebarsTemplateProcessorModel.empty, theTree, hmrcDms, LangADT.En)
        .expectDestinationAudit(hmrcDms, None, None, si.formId, pdfData, si.submission.submissionRef, template, model)
        .sut
        .submitIfIncludeIf(
          hmrcDms,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation(
            List(DestinationResult(hmrcDms.id, None, Some(si.customerId), None, None, None, None, None))
          ),
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "be sent to the DmsSubmitter when includeIf is false" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen,
      formTemplateGen,
      pdfDataGen,
      instructionPdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, instructionPdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel()
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          instructionPdfData,
          structuredFormData
        )

      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(hmrcDms.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          requiredResult = false
        )
        .sut
        .submitIfIncludeIf(
          hmrcDms,
          si,
          model,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(includeIf = Some(HandlebarValue(true.toString)), failOnError = Some(false)),
      formTemplateGen,
      pdfDataGen,
      instructionPdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, instructionPdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          instructionPdfData,
          structuredFormData
        )

      createSubmitter
        .expectHmrcDmsSubmissionFailure(
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          hmrcDms,
          "an error",
          LangADT.En
        )
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
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation(
            List(DestinationResult(hmrcDms.id, None, Some(si.customerId), None, None, None, None, None))
          ),
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionInfoGen,
      hmrcDmsGen(failOnError = Some(true), includeIf = Some(HandlebarValue(true.toString))),
      formTemplateGen,
      pdfDataGen,
      instructionPdfDataGen,
      structureFormValueObjectStructureGen
    ) { (si, hmrcDms, template, pdfData, instructionPdfData, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val theTree =
        tree(
          si.formId,
          model,
          si.submission.submissionRef,
          template,
          pdfData,
          instructionPdfData,
          structuredFormData
        )

      val res: Possible[Option[HandlebarsDestinationResponse]] = createSubmitter
        .expectHmrcDmsSubmissionFailure(
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          hmrcDms,
          "an error",
          LangADT.En
        )
        .expectIncludeIfEvaluation(
          "true",
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(theTree),
          true
        )
        .sut
        .submitIfIncludeIf(
          hmrcDms,
          si,
          HandlebarsTemplateProcessorModel.empty,
          theTree,
          submitter,
          None,
          LangADT.En,
          DestinationEvaluation(
            List(DestinationResult(hmrcDms.id, None, Some(si.customerId), None, None, None, None, None))
          ),
          UserSession.empty
        )

      res match {
        case Left(error) => error.getMessage shouldBe genericLogMessage(si.formId, hmrcDms.id, "an error")
        case Right(_)    => fail("Expected fail")
      }
    }
  }

  "A submission to destination SubmissionConsolidator" should "send to submission-consolidator when includeIf is true" in {
    forAll(
      submissionConsolidatorGen.map(_.copy(includeIf = HandlebarValue("true"))),
      submissionInfoGen,
      formTemplateGen,
      structureFormValueObjectStructureGen
    ) { (submissionConsolidator, submissionInfo, formTemplate, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val modelTree = tree(
        submissionInfo.formId,
        model,
        submissionInfo.submission.submissionRef,
        formTemplate,
        PdfContent(""),
        None,
        structuredFormData
      )
      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(submissionConsolidator.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(modelTree),
          true
        )
        .expectSubmissionConsolidatorSubmission(submissionConsolidator, submissionInfo, model, modelTree, formData)
        .expectDestinationAudit(
          submissionConsolidator,
          None,
          None,
          submissionInfo.formId,
          PdfContent(""),
          submissionInfo.submission.submissionRef,
          formTemplate,
          model
        )
        .sut
        .submitIfIncludeIf(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          submitter,
          Some(formData),
          LangADT.En,
          DestinationEvaluation(
            List(
              DestinationResult(
                submissionConsolidator.id,
                None,
                Some(submissionInfo.customerId),
                None,
                None,
                None,
                None,
                None
              )
            )
          ),
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "not be sent to the submission-consolidator when includeIf is false" in {
    forAll(
      submissionConsolidatorGen.map(_.copy(includeIf = HandlebarValue("false"))),
      submissionInfoGen,
      formTemplateGen,
      structureFormValueObjectStructureGen
    ) { (submissionConsolidator, submissionInfo, formTemplate, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val modelTree = tree(
        submissionInfo.formId,
        model,
        submissionInfo.submission.submissionRef,
        formTemplate,
        PdfContent(""),
        None,
        structuredFormData
      )
      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(submissionConsolidator.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(modelTree),
          false
        )
        .sut
        .submitIfIncludeIf(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          submitter,
          Some(formData),
          LangADT.En,
          DestinationEvaluation.empty,
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "return without raising an error if the endpoint returns an error but failOnError is false" in {

    forAll(
      submissionConsolidatorGen.map(_.copy(includeIf = HandlebarValue("true"), failOnError = false)),
      submissionInfoGen,
      formTemplateGen,
      structureFormValueObjectStructureGen
    ) { (submissionConsolidator, submissionInfo, formTemplate, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val modelTree = tree(
        submissionInfo.formId,
        model,
        submissionInfo.submission.submissionRef,
        formTemplate,
        PdfContent(""),
        None,
        structuredFormData
      )
      createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(submissionConsolidator.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(modelTree),
          true
        )
        .expectSubmissionConsolidatorSubmissionFailure(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          form,
          "some error"
        )
        .expectDestinationAudit(
          submissionConsolidator,
          None,
          None,
          submissionInfo.formId,
          PdfContent(""),
          submissionInfo.submission.submissionRef,
          formTemplate,
          model
        )
        .sut
        .submitIfIncludeIf(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          submitter,
          Some(formData),
          LangADT.En,
          DestinationEvaluation(
            List(
              DestinationResult(
                submissionConsolidator.id,
                None,
                Some(submissionInfo.customerId),
                None,
                None,
                None,
                None,
                None
              )
            )
          ),
          UserSession.empty
        ) shouldBe Right(None)
    }
  }

  it should "raise a failure if the endpoint returns an error and failOnError is true" in {
    forAll(
      submissionConsolidatorGen.map(_.copy(includeIf = HandlebarValue("true"), failOnError = true)),
      submissionInfoGen,
      formTemplateGen,
      structureFormValueObjectStructureGen
    ) { (submissionConsolidator, submissionInfo, formTemplate, structuredFormData) =>
      val model = HandlebarsTemplateProcessorModel.empty
      val modelTree = tree(
        submissionInfo.formId,
        model,
        submissionInfo.submission.submissionRef,
        formTemplate,
        PdfContent(""),
        None,
        structuredFormData
      )
      val res = createSubmitter
        .expectIncludeIfEvaluation(
          getHandlebarValue(submissionConsolidator.includeIf),
          HandlebarsTemplateProcessorModel.empty,
          FocussedHandlebarsModelTree(modelTree),
          true
        )
        .expectSubmissionConsolidatorSubmissionFailure(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          form,
          "some error"
        )
        .sut
        .submitIfIncludeIf(
          submissionConsolidator,
          submissionInfo,
          model,
          modelTree,
          submitter,
          Some(formData),
          LangADT.En,
          DestinationEvaluation(
            List(
              DestinationResult(
                submissionConsolidator.id,
                None,
                Some(submissionInfo.customerId),
                None,
                None,
                None,
                None,
                None
              )
            )
          ),
          UserSession.empty
        )

      res match {
        case Left(error) =>
          error.getMessage shouldBe genericLogMessage(submissionInfo.formId, submissionConsolidator.id, "some error")
        case Right(_) => fail("Expected fail")
      }
    }
  }

  case class SubmitterParts[F[_]](
    sut: DestinationSubmitter[F],
    dmsSubmitter: DmsSubmitterAlgebra[F],
    handlebarsSubmitter: HandlebarsHttpApiSubmitter,
    destinationAuditer: DestinationAuditAlgebra[F],
    handlebarsTemplateProcessor: HandlebarsTemplateProcessor,
    submissionConsolidatorService: SubmissionConsolidatorAlgebra[F],
    infoArchiveSubmitter: InfoArchiveSubmitterAlgebra[F]
  )(implicit F: MonadError[F, Throwable]) {

    def expectHmrcDmsSubmission(
      si: DestinationSubmissionInfo,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      modelTree: HandlebarsModelTree,
      hmrcDms: HmrcDms,
      l: LangADT
    )(implicit F: Applicative[F]): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(
          _: DestinationSubmissionInfo,
          _: HandlebarsTemplateProcessorModel,
          _: HandlebarsModelTree,
          _: HmrcDms,
          _: LangADT
        )(_: HeaderCarrier))
        .expects(si, accumulatedModel, modelTree, hmrcDms, l, hc)
        .returning(F.pure(()))
      this
    }

    def expectSubmissionConsolidatorSubmissionFailure(
      destination: SubmissionConsolidator,
      submissionInfo: DestinationSubmissionInfo,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      model: HandlebarsModelTree,
      form: Form,
      error: String
    ): SubmitterParts[F] = {
      (
        submissionConsolidatorService
          .submit(
            _: SubmissionConsolidator,
            _: DestinationSubmissionInfo,
            _: HandlebarsTemplateProcessorModel,
            _: HandlebarsModelTree,
            _: Option[FormData]
          )(_: HeaderCarrier)
        )
        .expects(destination, submissionInfo, accumulatedModel, model, Some(formData), hc)
        .returning(F.raiseError(new Exception(error)))
      this
    }

    def expectInfoArchiveSubmission(
      si: DestinationSubmissionInfo,
      modelTree: HandlebarsModelTree,
      result: Option[DestinationResult],
      infoArchive: InfoArchive,
      l: LangADT
    )(implicit F: Applicative[F]): SubmitterParts[F] = {
      (infoArchiveSubmitter
        .apply(
          _: DestinationSubmissionInfo,
          _: HandlebarsModelTree,
          _: Option[DestinationResult],
          _: InfoArchive,
          _: LangADT
        )(_: HeaderCarrier))
        .expects(si, modelTree, result, infoArchive, l, hc)
        .returning(F.pure(()))
      this
    }

    def expectSubmissionConsolidatorSubmission(
      destination: SubmissionConsolidator,
      submissionInfo: DestinationSubmissionInfo,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      model: HandlebarsModelTree,
      formData: FormData
    ): SubmitterParts[F] = {
      (
        submissionConsolidatorService
          .submit(
            _: SubmissionConsolidator,
            _: DestinationSubmissionInfo,
            _: HandlebarsTemplateProcessorModel,
            _: HandlebarsModelTree,
            _: Option[FormData]
          )(_: HeaderCarrier)
        )
        .expects(destination, submissionInfo, accumulatedModel, model, Some(formData), hc)
        .returning(F.pure(()))
      this
    }

    def expectHmrcDmsSubmissionFailure(
      si: DestinationSubmissionInfo,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      model: HandlebarsModelTree,
      hmrcDms: HmrcDms,
      error: String,
      l: LangADT
    ): SubmitterParts[F] = {
      (dmsSubmitter
        .apply(
          _: DestinationSubmissionInfo,
          _: HandlebarsTemplateProcessorModel,
          _: HandlebarsModelTree,
          _: HmrcDms,
          _: LangADT
        )(_: HeaderCarrier))
        .expects(si, accumulatedModel, model, hmrcDms, l, hc)
        .returning(F.raiseError(new Exception(error)))
      this
    }

    def expectHandlebarsSubmission(
      handlebarsHttpApi: Destination.HandlebarsHttpApi,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      tree: HandlebarsModelTree,
      response: HttpResponse,
      sdi: DestinationSubmissionInfo
    ): SubmitterParts[F] = {
      (handlebarsSubmitter
        .apply(
          _: Destination.HandlebarsHttpApi,
          _: HandlebarsTemplateProcessorModel,
          _: HandlebarsModelTree,
          _: DestinationSubmissionInfo
        )(
          _: HeaderCarrier
        ))
        .expects(handlebarsHttpApi, accumulatedModel, tree, sdi, hc)
        .returning(Future(response))
      this
    }

    def expectIncludeIfEvaluation(
      expression: String,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      tree: FocussedHandlebarsModelTree,
      requiredResult: Boolean
    ): SubmitterParts[F] = {
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
      pdfHtml: PdfContent,
      submissionRef: SubmissionRef,
      template: FormTemplate,
      model: HandlebarsTemplateProcessorModel
    ): SubmitterParts[F] = {
      (destinationAuditer
        .apply(
          _: Destination,
          _: Option[Int],
          _: Option[String],
          _: FormId,
          _: PdfContent,
          _: SubmissionRef,
          _: FormTemplate,
          _: HandlebarsTemplateProcessorModel
        )(_: HeaderCarrier))
        .expects(destination, responseCode, responseBody, formId, pdfHtml, submissionRef, template, model, hc)
        .returning(F.pure(()))
      this
    }
  }

  private def createSubmitter: SubmitterParts[Possible] = {
    val dmsSubmitter = mock[DmsSubmitterAlgebra[Possible]]
    val handlebarsSubmitter = mock[HandlebarsHttpApiSubmitter]
    val stateTransitionService = mock[StateTransitionAlgebra[Possible]]
    val handlebarsTemplateProcessor = mock[HandlebarsTemplateProcessor]
    val notifierService = mock[NotifierAlgebra[Possible]]
    val destinationAuditer = mock[DestinationAuditAlgebra[Possible]]
    val submissionConsolidator = mock[SubmissionConsolidatorAlgebra[Possible]]
    val dataStoreSubmitter = mock[DataStoreSubmitterAlgebra[Possible]]
    val infoArchiveSubmitter = mock[InfoArchiveSubmitterAlgebra[Possible]]
    val sdesRouting = SdesRouting("api-key", "information-type", "recipient-or-sender")
    val defaults = WelshDefaults("WLU-WCC-XDFSWelshLanguageService", "WLU")
    val sdesConfig =
      SdesConfig("base-path", "file-location-url", sdesRouting, sdesRouting, sdesRouting, 100L, defaults, sdesRouting)
    val submitter =
      new DestinationSubmitter[Possible](
        dmsSubmitter,
        handlebarsSubmitter,
        stateTransitionService,
        notifierService,
        Some(destinationAuditer),
        submissionConsolidator,
        dataStoreSubmitter,
        infoArchiveSubmitter,
        sdesConfig,
        handlebarsTemplateProcessor
      )

    SubmitterParts(
      submitter,
      dmsSubmitter,
      handlebarsSubmitter,
      destinationAuditer,
      handlebarsTemplateProcessor,
      submissionConsolidator,
      infoArchiveSubmitter
    )
  }

  private def submitter: DestinationsSubmitter[Possible] = {
    val destinationSubmitter: DestinationSubmitterAlgebra[Possible] = mock[DestinationSubmitterAlgebra[Possible]]
    new DestinationsSubmitter[Possible](destinationSubmitter)
  }

  def tree(
    id: FormId,
    model: HandlebarsTemplateProcessorModel,
    submissionRef: SubmissionRef,
    formTemplate: FormTemplate,
    pdfData: PdfContent,
    instructionPdfData: Option[PdfContent],
    structuredFormData: StructuredFormValue.ObjectStructure
  ) =
    HandlebarsModelTree(
      id,
      submissionRef,
      formTemplate,
      pdfData,
      instructionPdfData,
      structuredFormData,
      model
    )
}
