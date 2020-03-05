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

package uk.gov.hmrc.gform.testonly

import cats.data.EitherT
import cats.instances.option._
import cats.syntax.eq._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import java.time.LocalDateTime

import akka.util.ByteString
import play.api.Logger
import play.api.http.HttpEntity
import play.api.libs.json._
import play.api.mvc._
import reactivemongo.api.DB
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.BuildInfo
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.DesAlgebra
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.submission.destinations.DestinationsProcessorModelAlgebra
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, RealHandlebarsTemplateProcessor }

class TestOnlyController(
  controllerComponents: ControllerComponents,
  mongo: () => DB,
  enrolmentConnector: EnrolmentConnector,
  formAlgebra: FormAlgebra[Future],
  formTemplateAlgebra: FormTemplateAlgebra[Future],
  destinationsModelProcessorAlgebra: DestinationsProcessorModelAlgebra[Future],
  des: DesAlgebra[Future])(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def getFromDes(url: String): Action[AnyContent] =
    Action.async { request =>
      val urlWithQueryString = url + Option(request.rawQueryString)
        .filterNot(_.isEmpty)
        .map(qs => s"?$qs")
        .getOrElse("")

      logInfo(s"TestOnlyController.getFromDes: $urlWithQueryString")

      des
        .testOnlyGet(urlWithQueryString)
        .map { response =>
          Result(
            header = ResponseHeader(response.status, Map.empty),
            body = HttpEntity.Strict(ByteString(response.body), None))
        }
        .recover {
          case t: Throwable =>
            Result(header = ResponseHeader(500, Map.empty), body = HttpEntity.Strict(ByteString(t.toString), None))
        }
    }

  def renderHandlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    formId: FormId
  ): Action[SubmissionData] =
    Action.async(parse.json[SubmissionData]) { implicit request =>
      logInfo("TestOnlyController.renderHandlebarPayload starting")

      val submissionData: SubmissionData = request.body
      logInfo("TestOnlyController.renderHandlebarPayload Got submission data")

      val customerId = request.headers.get("customerId").getOrElse("")

      logInfo("TestOnlyController.renderHandlebarPayload Getting bits and pieces")
      for {
        formTemplate <- formTemplateAlgebra.get(formTemplateId)
        _ = logInfo("TestOnlyController.renderHandlebarPayload Got form template")
        form <- formAlgebra.get(formId)
        _ = logInfo("TestOnlyController.renderHandlebarPayload Got form")
        submission = Submission(
          formId,
          LocalDateTime.now(),
          SubmissionRef(form.envelopeId),
          form.envelopeId,
          0,
          DmsMetaData(formTemplate._id, customerId)
        )
        _ = logInfo("TestOnlyController.renderHandlebarPayload Got submission")
        model <- destinationsModelProcessorAlgebra
                  .create(form, submissionData.variables, submissionData.pdfData, submissionData.structuredFormData)
        _ = logInfo("TestOnlyController.renderHandlebarPayload Got model")
      } yield {
        val maybeDestination: Option[Destination.HandlebarsHttpApi] =
          findHandlebarsDestinationWithId(destinationId, formTemplate.destinations)

        val availableDestinationIds: String =
          availableHandlebarsDestinations(formTemplate.destinations).map(_.id.id).mkString(", ")

        val resultPayload: EitherT[Option, String, String] =
          for {
            destination <- fromOption(
                            maybeDestination,
                            s"No handlebars destination '${destinationId.id}' found on formTemplate '${formTemplateId.value}'. Available handlebars destinations: $availableDestinationIds."
                          ).leftMap { s =>
                            logInfo(s); s
                          }
            payload <- fromOption(
                        destination.payload,
                        s"There is no payload field on destination '${destinationId.id}' for formTemplate '${formTemplateId.value}'")
                        .leftMap { s =>
                          logInfo(s); s
                        }
          } yield {
            logInfo("TestOnlyController.renderHandlebarPayload Rendering")
            RealHandlebarsTemplateProcessor(
              payload,
              HandlebarsTemplateProcessorModel.empty,
              FocussedHandlebarsModelTree(
                HandlebarsModelTree(
                  submission._id,
                  submission.submissionRef,
                  formTemplate,
                  submissionData.pdfData,
                  submissionData.structuredFormData,
                  model)),
              destination.payloadType
            )
          }

        logInfo(s"TestOnlyController.renderHandlebarPayload Complete with ${resultPayload.map(_ => "Success").merge}")

        resultPayload.fold(BadRequest(_), Ok(_)).getOrElse(BadRequest("Oops, something went wrong"))
      }
    }

  private def availableHandlebarsDestinations(destinations: Destinations): List[Destination.HandlebarsHttpApi] =
    destinations match {
      case DestinationList(list, acknowledgementSection) => availableHandlebarsDestinations(list.toList)

      case _ => List.empty
    }

  private def availableHandlebarsDestinations(
    destinations: List[Destination],
    acc: List[Destination.HandlebarsHttpApi] = Nil): List[Destination.HandlebarsHttpApi] = destinations match {
    case Nil => acc
    case (head: Destination.Composite) :: tail =>
      availableHandlebarsDestinations(tail, availableHandlebarsDestinations(head.destinations.toList, acc))
    case (head: Destination.HandlebarsHttpApi) :: tail => availableHandlebarsDestinations(tail, head :: acc)
    case _ :: tail                                     => availableHandlebarsDestinations(tail, acc)
  }

  private def findHandlebarsDestinationWithId(
    id: DestinationId,
    destinations: Destinations): Option[Destination.HandlebarsHttpApi] =
    availableHandlebarsDestinations(destinations).find(_.id === id)

  private def fromOption[A, B](a: Option[A], s: B): EitherT[Option, B, A] = EitherT.fromOption(a, s)

  private lazy val formTemplates = mongo().collection[JSONCollection]("formTemplate")
  def removeTemplates() = Action.async { implicit request =>
    println("purging mongo database ....")
    formTemplates.drop(failIfNotFound = false).map(_ => Results.Ok("Mongo purged")).recover {

      case e =>
        e.printStackTrace()
        Results.InternalServerError(e.toString)
    }
  }

  def buildInfo() = Action { r =>
    Results.Ok(Json.toJson(BuildInfo.toMap.mapValues(_.toString)))

  }

  case class User(id: String, postCode: String, countryCode: String)

  object User {
    val reads: Reads[User] = Json.format[User]
    val write: OWrites[User] = OWrites[User] { o =>
      getJson(o)
    }

    implicit val format = OFormat[User](reads, write)
  }

  def config() = Action { r =>
    val result: JsValue = Json.parse(ConfigFactory.load().root().render(ConfigRenderOptions.concise()))
    Results.Ok(result)
  }

  def getJson(user: User): JsObject =
    if (user.postCode.nonEmpty) {
      Json.obj(
        "verifiers" -> Json.arr(
          Json.obj("key" -> "NonUkCountryCode", "value" -> user.countryCode),
          Json.obj("key" -> "BusinessPostcode", "value" -> user.postCode))) //{"verifiers" : [{"key" : "NonUkCountryCode","value" : "GB"},{"key" : "BusinessPostcode","value" : "E499OL"}]}
    } else {
      Json.obj("verifiers" -> Json.arr(Json.obj("key" -> "NonUkCountryCode", "value" -> user.countryCode)))
    }

  def upload = Action.async(parse.json[User]) { implicit request =>
    val user: User = request.body
    enrolmentConnector.upload(user.id, Json.toJson(user)).map(_ => NoContent)
  }

  def deEnrolUser(userId: String) = Action.async(parse.json[User]) { implicit request =>
    val user = request.body
    enrolmentConnector.deEnrol(userId, user.id).map(x => Ok(x.body))
  }

  def delete = Action.async(parse.json[User]) { implicit request =>
    val user = request.body
    enrolmentConnector.removeUnallocated(user.id).map(_ => NoContent)
  }

  private def logInfo(message: String): Unit =
    Logger.info(message)
}
