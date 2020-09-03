/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submissionconsolidator

import java.time.format.DateTimeFormatter
import cats.syntax.either._
import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ Matchers, WordSpecLike }
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.PdfHtml
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormId, FormStatus, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.SubmissionConsolidator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, HandlebarsTemplateProcessorModel, JsonNodes }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.DestinationGen.submissionConsolidatorGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormTemplateGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate }
import uk.gov.hmrc.gform.sharedmodel.generators.StructuredFormValueGen._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.TextNode
import uk.gov.hmrc.gform.submission.destinations.DestinationSubmissionInfo
import uk.gov.hmrc.gform.submission.destinations.DestinationSubmissionInfoGen._
import uk.gov.hmrc.gform.submission.handlebars.{ HandlebarsModelTree, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SubmissionConsolidatorServiceSpec
    extends WordSpecLike with MockFactory with Matchers with PropertyChecks with ScalaFutures {

  override implicit val patienceConfig = PatienceConfig(Span(10, Seconds), Span(1, Millis))
  private val DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  case class TestData(
    submissionConsolidator: SubmissionConsolidator,
    destinationSubmissionInfo: DestinationSubmissionInfo,
    formData: FormData,
    formTemplate: FormTemplate,
    structuredFormData: StructuredFormValue.ObjectStructure,
    model: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree)

  trait TestFixture {
    val mockSubmissionConsolidatorConnector = mock[SubmissionConsolidatorConnector]
    val mockFormService = mock[FormAlgebra[FOpt]]
    val submissionConsolidatorService =
      new SubmissionConsolidatorService(
        RealHandlebarsTemplateProcessor,
        mockSubmissionConsolidatorConnector,
        mockFormService)
  }

  "submit" when {
    "destination has no formData template" should {
      "send submission with all form fields" in new TestFixture {
        forAll(testDataGen) { testData =>
          import testData._
          val destination = submissionConsolidator.copy(formData = None)
          (mockSubmissionConsolidatorConnector
            .sendForm(_: SCForm)(_: HeaderCarrier))
            .expects(expectedAPIForm(destinationSubmissionInfo, formData, destination, structuredFormData), hc)
            .returns(Future.successful(Right(())))
          (mockFormService
            .updateFormStatus(_: FormId, _: FormStatus)(_: HeaderCarrier))
            .expects(destinationSubmissionInfo.formId, Submitted, hc)
            .returns(EitherT(Future.successful[Either[UnexpectedState, FormStatus]](Right(Submitted))))
          val future = submissionConsolidatorService
            .submit(destination, destinationSubmissionInfo, model, modelTree, Some(formData))
            .value

          future.futureValue shouldBe Right(())
        }
      }
    }

    "destination has formData template" should {
      "send submission with fields from formData template" in new TestFixture {
        forAll(testDataGen) { testData =>
          import testData._
          (mockSubmissionConsolidatorConnector
            .sendForm(_: SCForm)(_: HeaderCarrier))
            .expects(
              expectedAPIForm(destinationSubmissionInfo, formData, submissionConsolidator, structuredFormData),
              hc)
            .returns(Future.successful(Right(())))
          (mockFormService
            .updateFormStatus(_: FormId, _: FormStatus)(_: HeaderCarrier))
            .expects(destinationSubmissionInfo.formId, Submitted, hc)
            .returns(EitherT(Future.successful[Either[UnexpectedState, FormStatus]](Right(Submitted))))
          val future = submissionConsolidatorService
            .submit(submissionConsolidator, destinationSubmissionInfo, model, modelTree, Some(formData))
            .value

          future.futureValue shouldBe Right(())
        }
      }
    }

    "destination formData template is invalid" should {
      "return error" in new TestFixture {
        forAll(testDataGen) { testData =>
          import testData._
          val destination = submissionConsolidator.copy(formData = Some("invalid_template"))
          (mockSubmissionConsolidatorConnector
            .sendForm(_: SCForm)(_: HeaderCarrier))
            .expects(*, *)
            .returns(Future.successful(Right(())))
            .never()

          val future = submissionConsolidatorService
            .submit(destination, destinationSubmissionInfo, model, modelTree, Some(formData))
            .value

          future.futureValue shouldBe Left(UnexpectedState(
            "Unrecognized token 'invalid_template': was expecting (JSON String, Number, Array, Object or token 'null', 'true' or 'false')\n at [Source: (String)\"invalid_template\"; line: 1, column: 17]"))
        }
      }
    }
  }

  private def expectedAPIForm(
    destinationSubmissionInfo: DestinationSubmissionInfo,
    formData: FormData,
    destination: Destination.SubmissionConsolidator,
    structuredFormData: StructuredFormValue.ObjectStructure) =
    SCForm(
      destinationSubmissionInfo.submission.submissionRef.value,
      destination.projectId.id,
      destinationSubmissionInfo.submission.dmsMetaData.formTemplateId.value,
      destinationSubmissionInfo.customerId,
      destinationSubmissionInfo.submission.submittedDate.format(DATE_TIME_FORMAT),
      destination.formData
        .map(_ => structuredFormData.fields.map(f => SCFormField(f.name.name, f.value.asInstanceOf[TextNode].value)))
        .getOrElse(formData.toData.toList.map {
          case (FormComponentId(id), value) => SCFormField(id, value)
        })
    )

  private val testDataGen = for {
    structuredFormData <- structureFormValueObjectStructureGen
    model = HandlebarsTemplateProcessorModel(
      structuredFormData.fields.map(f => (f.name.name, JsonNodes.textNode(f.value.asInstanceOf[TextNode].value))): _*)
    submissionConsolidator <- submissionConsolidatorGen.map(d =>
                               d.copy(formData = Some(s"""[${structuredFormData.fields
                                 .map { f =>
                                   s"""{"id": "${f.name.name}", "value": "{{${f.name.name}}}"}"""
                                 }
                                 .mkString(",")}]""")))
    formTemplate              <- formTemplateGen
    formData                  <- formDataGen
    destinationSubmissionInfo <- destinationSubmissionInfoGen
    modelTree = HandlebarsModelTree(
      destinationSubmissionInfo.formId,
      destinationSubmissionInfo.submission.submissionRef,
      formTemplate,
      PdfHtml(""),
      structuredFormData,
      model)
  } yield
    TestData(
      submissionConsolidator,
      destinationSubmissionInfo,
      formData,
      formTemplate,
      structuredFormData,
      model,
      modelTree)
}
