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

package uk.gov.hmrc.gform.testonly

import cats.syntax.traverse._
import cats.data.EitherT
import cats.instances.option._
import cats.instances.future._
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import java.time.LocalDateTime

import play.api.libs.json._
import play.api.mvc._
import reactivemongo.api.DB
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.BuildInfo
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.fileupload.{ FileDownloadAlgebra, UploadedFile }
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DmsSubmission }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.submission.{ DestinationSubmissionInfo, DestinationsSubmitter, DmsMetaData, Submission, SubmissionRef }
import uk.gov.hmrc.gform.submission.handlebars.RealHandlebarsTemplateProcessor
import uk.gov.hmrc.http.HeaderCarrier

class TestOnlyController(
  mongo: () => DB,
  enrolmentConnector: EnrolmentConnector,
  formAlgebra: FormAlgebra[Future],
  formTemplateAlgebra: FormTemplateAlgebra[Future],
  fileDownloadAlgebra: Option[FileDownloadAlgebra[Future]])(implicit ex: ExecutionContext)
    extends BaseController {

  def renderHandlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    formId: FormId
  ): Action[SubmissionData] =
    Action.async(parse.json[SubmissionData]) { implicit request =>
      val submissionData: SubmissionData = request.body

      val customerId = request.headers.get("customerId").getOrElse("")
      val affinityGroup = toAffinityGroupO(request.headers.get("affinityGroup"))

      for {
        formTemplate <- formTemplateAlgebra.get(formTemplateId)
        form         <- formAlgebra.get(formId)
        submission = Submission(
          formId,
          LocalDateTime.now(),
          SubmissionRef(form.envelopeId),
          form.envelopeId,
          0,
          DmsMetaData(formTemplate._id, customerId)
        )
        files <- uploadedFiles(form.envelopeId)
        submissionInfo = DestinationSubmissionInfo(formId, customerId, affinityGroup, submissionData, submission)
      } yield {
        val model: HandlebarsTemplateProcessorModel =
          DestinationsSubmitter.createHandlebarsTemplateProcessorModel(submissionInfo, form, files)

        val maybeDestination: Option[Destination.HandlebarsHttpApi] = formTemplate.destinations match {
          case DestinationList(destinations) =>
            destinations.toList.collectFirst {
              case d @ Destination.HandlebarsHttpApi(`destinationId`, _, _, _, _, _, _, _) => d
            }
          case _ => None
        }

        val availableDestinationIds: String = formTemplate.destinations match {
          case DestinationList(destinations)           => destinations.toList.map(_.id.id).mkString(", ")
          case DmsSubmission(dmsFormId, _, _, _, _, _) => dmsFormId
        }

        val resultPayload: EitherT[Option, String, String] =
          for {
            destination <- fromOption(
                            maybeDestination,
                            s"No destination '${destinationId.id}' found on formTemplate '${formTemplateId.value}'. Available destinations: $availableDestinationIds."
                          )
            payload <- fromOption(
                        destination.payload,
                        s"There is no payload field on destination '${destinationId.id}' for formTemplate '${formTemplateId.value}'")
          } yield RealHandlebarsTemplateProcessor(payload, model, destination.payloadType)

        resultPayload.fold(BadRequest(_), Ok(_)).getOrElse(BadRequest("Ups, something went wrong"))
      }
    }

  private def fromOption[A, B](a: Option[A], s: B): EitherT[Option, B, A] = EitherT.fromOption(a, s)

  private def uploadedFiles(envelopedId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Option[List[UploadedFile]]] =
    fileDownloadAlgebra.traverse { _.allUploadedFiles(envelopedId) }

  lazy val formTemplates = mongo().collection[JSONCollection]("formTemplate")
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

}
