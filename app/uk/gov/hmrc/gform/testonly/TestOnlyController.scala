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

package uk.gov.hmrc.gform.testonly

import cats.syntax.eq._
import com.fasterxml.jackson.databind.JsonNode
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }

import java.time.{ LocalDateTime, ZoneId }
import akka.util.ByteString
import org.apache.commons.text.StringEscapeUtils
import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.DesAlgebra
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.FormTemplateAlgebra
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.TemplateType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations, HandlebarsTemplateProcessorModel }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.destinations.{ DataStoreSubmitter, DestinationSubmissionInfo, DestinationsProcessorModelAlgebra }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.http.BuildInfo
import uk.gov.hmrc.mongo.MongoComponent

class TestOnlyController(
  controllerComponents: ControllerComponents,
  mongoComponent: MongoComponent,
  enrolmentConnector: EnrolmentConnector,
  formAlgebra: FormAlgebra[Future],
  formTemplateAlgebra: FormTemplateAlgebra[Future],
  destinationsModelProcessorAlgebra: DestinationsProcessorModelAlgebra[Future],
  dataStoreSubmitter: DataStoreSubmitter,
  des: DesAlgebra[Future],
  testOnlyFormService: TestOnlyFormService
)(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  private val formTemplatesRepo = new Repo[FormTemplate]("formTemplate", mongoComponent, _._id.value)

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
            body = HttpEntity.Strict(ByteString(response.body), None)
          )
        }
        .recover { case t: Throwable =>
          Result(header = ResponseHeader(500, Map.empty), body = HttpEntity.Strict(ByteString(t.toString), None))
        }
    }

  def renderHandlebarModel(
    formTemplateId: FormTemplateId,
    formId: FormId
  ): Action[SubmissionData] =
    Action.async(parse.json[SubmissionData]) { implicit request =>
      val submissionData: SubmissionData = request.body

      for {
        formTemplate <- formTemplateAlgebra.get(formTemplateId)
        form         <- formAlgebra.get(formId)
        model <- destinationsModelProcessorAlgebra
                   .create(
                     form,
                     submissionData.variables,
                     submissionData.pdfData,
                     submissionData.instructionPDFData,
                     submissionData.structuredFormData,
                     formTemplate.isObjectStore
                   )
      } yield {
        val jsValue: JsValue = Json.toJson[JsonNode](model.model)
        Ok(jsValue)
      }
    }

  def renderHandlebarPayload(
    formTemplateId: FormTemplateId,
    destinationId: DestinationId,
    formId: FormId
  ): Action[SubmissionData] =
    Action.async(parse.json[SubmissionData]) { implicit request =>
      val submissionData: SubmissionData = request.body

      for {
        formTemplate <- formTemplateAlgebra.get(formTemplateId)
        form         <- formAlgebra.get(formId)
        submission = Submission(
                       SubmissionId(formId, form.envelopeId),
                       LocalDateTime.now(ZoneId.of("Europe/London")),
                       SubmissionRef(form.envelopeId),
                       form.envelopeId,
                       0,
                       DmsMetaData(formTemplate._id, customerIdHeader)
                     )
        model <- destinationsModelProcessorAlgebra
                   .create(
                     form,
                     submissionData.variables,
                     submissionData.pdfData,
                     submissionData.instructionPDFData,
                     submissionData.structuredFormData,
                     formTemplate.isObjectStore
                   )
      } yield {
        val maybeDestination: Option[Destination] =
          findHandlebarsDestinationWithId(destinationId, formTemplate.destinations)

        val destinationResult: Option[DestinationResult] =
          submissionData.destinationEvaluation.evaluation.find(_.destinationId === destinationId)

        maybeDestination match {
          case None => BadRequest(s"No destination '${destinationId.id}' found")
          case Some(destination) =>
            val modelTree = HandlebarsModelTree(
              submission._id.formId,
              submission.submissionRef,
              formTemplate,
              submissionData.pdfData,
              submissionData.instructionPDFData,
              submissionData.structuredFormData,
              model
            )
            destination match {
              case d: Destination.HandlebarsHttpApi =>
                d.payload match {
                  case None => BadRequest(s"Destination '${d.id.id}' is missing payload data")
                  case Some(payload) =>
                    val res = RealHandlebarsTemplateProcessor(
                      payload,
                      HandlebarsTemplateProcessorModel.empty,
                      FocussedHandlebarsModelTree(modelTree),
                      d.payloadType
                    )
                    Ok(res)
                }
              case dataStore: Destination.DataStore =>
                val submissionInfo: DestinationSubmissionInfo = DestinationSubmissionInfo(customerIdHeader, submission)
                val structuredFormData: StructuredFormValue.ObjectStructure = submissionData.structuredFormData
                val userSession: UserSession = submissionData.userSession
                val taxpayerId: Option[String] = destinationResult.flatMap(_.taxpayerId)
                val accumulatedModel: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel.empty

                val tryResult = Try(
                  dataStoreSubmitter.generatePayload(
                    submissionInfo,
                    structuredFormData,
                    dataStore,
                    LangADT.En,
                    userSession,
                    taxpayerId,
                    accumulatedModel,
                    modelTree
                  )
                )

                tryResult.fold(
                  error => BadRequest(flattenExceptionMessage(error).mkString("\n")),
                  res => Ok(res)
                )

              case _ =>
                BadRequest(
                  "Invalid destination. Only 'handlebarsHttpApi' and 'hmrcIlluminate' destinations are allowed here"
                )
            }
        }
      }
    }

  private def findHandlebarsDestinationWithId(
    id: DestinationId,
    destinations: Destinations
  ): Option[Destination] =
    destinations match {
      case dl: Destinations.DestinationList =>
        dl.destinations.find {
          case (d: Destination.HandlebarsHttpApi) if d.id === id => true
          case (d: Destination.DataStore) if d.id === id         => true
          case _                                                 => false
        }
      case _: Destinations.DestinationPrint => None
    }

  def removeTemplates() = Action.async { _ =>
    logger.info("purging mongo database ....")
    formTemplatesRepo.collection.drop().toFuture().map(_ => Results.Ok("Mongo purged")).recover { case e =>
      e.printStackTrace()
      Results.InternalServerError(e.toString)
    }
  }

  def buildInfo() = Action { r =>
    Results.Ok(Json.toJson(BuildInfo.toMap.view.mapValues(_.toString)))
  }

  private def flattenExceptionMessage(ex: Throwable): List[String] = {
    def loop(ex: Throwable, acc: List[String]): List[String] =
      if (ex == null) acc
      else loop(ex.getCause, ex.getMessage :: acc)
    loop(ex.getCause, ex.getMessage :: Nil)
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
          Json.obj("key" -> "BusinessPostcode", "value" -> user.postCode)
        )
      ) //{"verifiers" : [{"key" : "NonUkCountryCode","value" : "GB"},{"key" : "BusinessPostcode","value" : "E499OL"}]}
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
    logger.info(message)

  private def customerIdHeader(implicit request: Request[_]): String =
    request.headers
      .get("customerId")
      .map(StringEscapeUtils.unescapeHtml4)
      .getOrElse("")

  def saveForm() =
    Action.async(parse.json[SaveRequest]) { request =>
      val saveRequest: SaveRequest = request.body
      testOnlyFormService.saveForm(saveRequest).map(saveReply => Ok(Json.toJson(saveReply)))
    }

  def restoreForm(snapshotId: String, restoreId: String) = Action.async { _ =>
    testOnlyFormService.restoreForm(snapshotId, restoreId).map(snapshot => Ok(Json.toJson(snapshot)))
  }

  def restoreCurrentForm(formId: String) =
    Action.async { request =>
      val savedId = request.body.asFormUrlEncoded.get("snapshotId").head
      testOnlyFormService.restoreForm(savedId, formId).map(_ => NoContent)
    }

  def getSnapshots() =
    Action.async { request =>
      testOnlyFormService.getSnapshots().map(snapshots => Ok(Json.toJson(snapshots)))
    }

  def getSnapshotData(snapshotId: String) = Action.async { _ =>
    testOnlyFormService.getSnapshotData(snapshotId).map(snapshotWithData => Ok(Json.toJson(snapshotWithData)))
  }

  def updateSnapshot() =
    Action.async(parse.json[UpdateSnapshotRequest]) { request =>
      val updateRequest: UpdateSnapshotRequest = request.body
      testOnlyFormService.updateSnapshot(updateRequest).map(saveReply => Ok(Json.toJson(saveReply)))
    }
}

final case class RenderableDestination(
  id: DestinationId,
  payload: Option[String],
  payloadType: TemplateType
)
