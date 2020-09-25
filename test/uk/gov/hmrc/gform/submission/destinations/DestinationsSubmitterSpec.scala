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

package uk.gov.hmrc.gform.submission.destinations

import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.{ Applicative, Monad }
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.gform.sharedmodel.generators.{ PdfDataGen, StructuredFormValueGen }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.sharedmodel.{ FrontEndSubmissionVariables, PdfHtml, SubmissionRef }
import uk.gov.hmrc.gform.submission.handlebars.HandlebarsModelTree
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class DestinationsSubmitterSpec
    extends Spec with DestinationGen with DestinationsGen with PdfDataGen with StructuredFormValueGen
    with ScalaCheckDrivenPropertyChecks {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map { si =>
      si.copy(submission = si.submission.copy(_id = form._id))
    }

  "Every Destination" should "be sent to the DestinationSubmitter" in {
    forAll(submissionInfoGen, destinationGen, pdfDataGen, structureFormValueObjectStructureGen) {
      (submissionInfo, destination, pdfData, structuredFormValue) =>
        val destinationModel = DestinationsProcessorModelAlgebra.createFormId(submissionInfo.formId)
        createSubmitter()
          .expectDestinationSubmitterSubmitIfIncludeIf(
            destination,
            submissionInfo,
            HandlebarsTemplateProcessorModel.empty,
            destinationModel,
            None,
            formData)
          .submitter
          .send(
            submissionInfo,
            HandlebarsModelTree(
              submissionInfo.formId,
              SubmissionRef(""),
              exampleTemplateWithDestinations(destination),
              pdfData,
              structuredFormValue,
              destinationModel),
            Some(formData)
          )
    }
  }

  "Subsequent Destination.HandlebarsHttpApi destinations" should "able to use the response codes and bodies from previous Destination.HandlebarsHttpApi destinations" in {
    forAll(
      submissionInfoGen,
      handlebarsHttpApiGen,
      handlebarsHttpApiGen,
      Gen.chooseNum(100, 599),
      pdfDataGen,
      structureFormValueObjectStructureGen
    ) { (submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2, responseCode1, pdfData, structuredFormValue) =>
      val responseJson1 = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val response1 = HttpResponse(responseCode1, Option(responseJson1))

      val initialModel = DestinationsProcessorModelAlgebra
        .createModel(FrontEndSubmissionVariables(JsNull), pdfData, structuredFormValue, form, None)

      val response1Model = HandlebarsDestinationResponse(handlebarsHttpApi1, response1)
      val accumulatedModel1 = DestinationsProcessorModelAlgebra.createDestinationResponse(response1Model)

      val response2Model = HandlebarsDestinationResponse(handlebarsHttpApi2, response1)

      createSubmitter()
        .expectDestinationSubmitterSubmitIfIncludeIf(
          handlebarsHttpApi1,
          submissionInfo,
          HandlebarsTemplateProcessorModel.empty,
          initialModel,
          Option(response1Model),
          formData)
        .expectDestinationSubmitterSubmitIfIncludeIf(
          handlebarsHttpApi2,
          submissionInfo,
          accumulatedModel1,
          initialModel,
          Option(response2Model),
          formData)
        .submitter
        .send(
          submissionInfo,
          HandlebarsModelTree(
            submissionInfo.formId,
            SubmissionRef(""),
            exampleTemplateWithDestinations(handlebarsHttpApi1, handlebarsHttpApi2),
            PdfHtml(""),
            StructuredFormValue.ObjectStructure(Nil),
            initialModel
          ),
          Some(formData)
        )
    }
  }

  "createResponseModel" should "build the appropriate JSON" in {
    forAll(handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val responseBody = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val responseModel =
        DestinationsProcessorModelAlgebra.createDestinationResponse(
          HandlebarsDestinationResponse(destination, HttpResponse(responseCode, Option(responseBody))))

      responseModel.model.toString shouldBe
        JsObject(
          Seq(
            s"${destination.id.id}" -> JsObject(
              Seq(
                "status" -> JsNumber(responseCode),
                "json"   -> responseBody
              ))
          )).toString
    }
  }

  it should "contain a JsNull when the response body is empty or cannot be parsed" in {
    forAll(handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val result = HandlebarsDestinationResponse(destination, HttpResponse(responseCode, responseString = Option("")))

      val responseModel = DestinationsProcessorModelAlgebra.createDestinationResponse(result)

      responseModel.model.toString shouldBe
        JsObject(
          Seq(
            s"${destination.id.id}" -> JsObject(
              Seq(
                "status" -> JsNumber(responseCode),
                "json"   -> JsNull
              ))
          )).toString
    }
  }

  private def exampleTemplateWithDestinations(destination1: Destination, moreDestinations: Destination*): FormTemplate =
    formTemplate.copy(
      destinations =
        Destinations.DestinationList(NonEmptyList.of(destination1, moreDestinations: _*), ackSection, decSection))

  private def exampleTemplateWithDestinations(destinations: Destinations): FormTemplate =
    formTemplate.copy(destinations = destinations)

  case class SubmitterParts[F[_]: Applicative](
    submitter: DestinationsSubmitter[F],
    destinationSubmitter: DestinationSubmitterAlgebra[F]) {

    def expectDestinationSubmitterSubmitIfIncludeIf(
      destination: Destination,
      submissionInfo: DestinationSubmissionInfo,
      accumulatedModel: HandlebarsTemplateProcessorModel,
      modelInTree: HandlebarsTemplateProcessorModel,
      response: Option[HandlebarsDestinationResponse],
      formData: FormData): SubmitterParts[F] = {
      (destinationSubmitter
        .submitIfIncludeIf(
          _: Destination,
          _: DestinationSubmissionInfo,
          _: HandlebarsTemplateProcessorModel,
          _: HandlebarsModelTree,
          _: DestinationsSubmitter[F],
          _: Option[FormData]
        )(_: HeaderCarrier))
        .expects(where {
          (
            dest: Destination,
            info: DestinationSubmissionInfo,
            accModel: HandlebarsTemplateProcessorModel,
            tree: HandlebarsModelTree,
            _: DestinationsSubmitter[F],
            actualformData: Option[FormData],
            hc: HeaderCarrier) =>
            destination === dest && info === submissionInfo && accModel === accumulatedModel && tree.value.model === modelInTree && hc === hc && actualformData
              .contains(formData)
        })
        .returning(response.pure)
      this
    }

    def expectSubmitToDms(
      destination: HmrcDms,
      submissionInfo: DestinationSubmissionInfo,
      pdfData: PdfHtml,
      structuredFormData: StructuredFormValue.ObjectStructure): SubmitterParts[F] = {
      (destinationSubmitter
        .submitToDms(_: DestinationSubmissionInfo, _: PdfHtml, _: StructuredFormValue.ObjectStructure, _: HmrcDms)(
          _: HeaderCarrier))
        .expects(submissionInfo, pdfData, structuredFormData, destination, hc)
        .returning(().pure)
      this
    }
  }

  private def createSubmitter[M[_]: Monad](): SubmitterParts[M] = {
    val destinationSubmitter: DestinationSubmitterAlgebra[M] = mock[DestinationSubmitterAlgebra[M]]
    val submitter = new DestinationsSubmitter[M](destinationSubmitter)

    SubmitterParts(submitter, destinationSubmitter)
  }
}
