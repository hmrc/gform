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
import cats.{ Applicative, Id, Monad }
import cats.syntax.applicative._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, Destinations, HandlebarsDestinationResponse, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ DestinationGen, DestinationsGen }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import org.scalacheck.Gen
import play.api.libs.json._
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.generators.DestinationSubmissionInfoGen

class DestinationsSubmitterSpec extends Spec {
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val formAlgebra = new FormAlgebra[Id] {
    override def get(formId: FormId)(implicit hc: HeaderCarrier): Id[Form] = form
    override def delete(formId: FormId)(implicit hc: HeaderCarrier): Id[Unit] = ???
    override def create(
      userId: UserId,
      formTemplateId: FormTemplateId,
      accessCode: Option[AccessCode],
      expiryDays: Long,
      initialFields: Seq[FormField])(implicit hc: HeaderCarrier): Id[FormId] = ???
    override def updateUserData(formId: FormId, userData: UserData)(implicit hc: HeaderCarrier): Id[Unit] = ???
    override def updateFormStatus(formId: FormId, newStatus: FormStatus)(implicit hc: HeaderCarrier): Id[FormStatus] =
      ???
    override def updateDestinationSubmissionInfo(formId: FormId, info: Option[DestinationSubmissionInfo])(
      implicit hc: HeaderCarrier): Id[Unit] = ???
    override def saveKeyStore(formId: FormId, data: Map[String, JsValue])(implicit hc: HeaderCarrier): Id[Unit] = ???
    override def getKeyStore(formId: FormId)(implicit hc: HeaderCarrier): Id[Option[Map[String, JsValue]]] = ???
  }

  private def submissionInfoGen: Gen[DestinationSubmissionInfo] =
    DestinationSubmissionInfoGen.destinationSubmissionInfoGen.map {
      _.copy(formId = form._id)
    }

  "Destinations.DmsSubmission" should "be sent to DestinationSubmitter" in {
    forAll(submissionInfoGen, DestinationsGen.deprecatedDmsSubmissionGen) { (submissionInfo, destination) =>
      createSubmitter()
        .expectSubmitToDms(destination, submissionInfo)
        .submitter
        .send(submissionInfo, exampleTemplateWithDestinations(destination), formAlgebra)
    }
  }

  "Every Destination" should "be sent to the DestinationSubmitter" in {
    forAll(submissionInfoGen, DestinationGen.destinationGen) { (submissionInfo, destination) =>
      createSubmitter()
        .expectDestinationSubmitterSubmitIfIncludeIf(
          destination,
          submissionInfo,
          DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo, form),
          None)
        .submitter
        .send(submissionInfo, exampleTemplateWithDestinations(destination), formAlgebra)
    }
  }

  "Subsequent Destination.HandlebarsHttpApi destinations" should "able to use the response codes and bodies from previous Destination.HandlebarsHttpApi destinations" in {
    forAll(
      submissionInfoGen,
      DestinationGen.handlebarsHttpApiGen,
      DestinationGen.handlebarsHttpApiGen,
      Gen.chooseNum(100, 599)
    ) { (submissionInfo, handlebarsHttpApi1, handlebarsHttpApi2, responseCode1) =>
      val responseJson1 = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val response1 = HttpResponse(responseCode1, Option(responseJson1))

      val initialModel = DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo, form)
      val response1Model = HandlebarsDestinationResponse(handlebarsHttpApi1, response1)
      val expectedModel2 = initialModel + HandlebarsTemplateProcessorModel(response1Model)

      createSubmitter()
        .expectDestinationSubmitterSubmitIfIncludeIf(
          handlebarsHttpApi1,
          submissionInfo,
          initialModel,
          Option(response1Model))
        .expectDestinationSubmitterSubmitIfIncludeIf(handlebarsHttpApi2, submissionInfo, expectedModel2, None)
        .submitter
        .send(submissionInfo, exampleTemplateWithDestinations(handlebarsHttpApi1, handlebarsHttpApi2), formAlgebra)
    }
  }

  "createResponseModel" should "build the appropriate JSON" in {
    forAll(DestinationGen.handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val responseBody = JsObject(
        Seq(
          "intField"    -> JsNumber(2),
          "stringField" -> JsString("stringNodeValue")
        ))

      val responseModel =
        HandlebarsTemplateProcessorModel(
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
    forAll(DestinationGen.handlebarsHttpApiGen, Gen.chooseNum(100, 599)) { (destination, responseCode) =>
      val result = HandlebarsDestinationResponse(destination, HttpResponse(responseCode, responseString = Option("")))

      val responseModel = HandlebarsTemplateProcessorModel(result)

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
    formTemplate.copy(destinations = Destinations.DestinationList(NonEmptyList.of(destination1, moreDestinations: _*)))

  private def exampleTemplateWithDestinations(destinations: Destinations): FormTemplate =
    formTemplate.copy(destinations = destinations)

  case class SubmitterParts[F[_]: Applicative](
    submitter: DestinationsSubmitter[F],
    destinationSubmitter: DestinationSubmitter[F]) {

    def expectDestinationSubmitterSubmitIfIncludeIf(
      destination: Destination,
      submissionInfo: DestinationSubmissionInfo,
      model: HandlebarsTemplateProcessorModel,
      response: Option[HandlebarsDestinationResponse]): SubmitterParts[F] = {
      (destinationSubmitter
        .submitIfIncludeIf(
          _: Destination,
          _: DestinationSubmissionInfo,
          _: HandlebarsTemplateProcessorModel,
          _: DestinationsSubmitter[F],
          _: FormTemplate)(_: HeaderCarrier))
        .expects(where {
          (
            dest: Destination,
            info: DestinationSubmissionInfo,
            model: HandlebarsTemplateProcessorModel,
            _: DestinationsSubmitter[F],
            _: FormTemplate,
            hc: HeaderCarrier) =>
            destination === dest && info === submissionInfo && model === model && hc === hc
        })
        .returning(response.pure)
      this
    }

    def expectSubmitToDms(
      destination: Destinations.DmsSubmission,
      submissionInfo: DestinationSubmissionInfo): SubmitterParts[F] = {
      (destinationSubmitter
        .submitToDms(_: DestinationSubmissionInfo, _: Destinations.DmsSubmission)(_: HeaderCarrier))
        .expects(submissionInfo, destination, hc)
        .returning(().pure)
      this
    }
  }

  private def createSubmitter[M[_]: Monad](): SubmitterParts[M] = {
    val destinationSubmitter: DestinationSubmitter[M] = mock[DestinationSubmitter[M]]
    val submitter = new DestinationsSubmitter[M](destinationSubmitter)

    SubmitterParts(submitter, destinationSubmitter)
  }
}
