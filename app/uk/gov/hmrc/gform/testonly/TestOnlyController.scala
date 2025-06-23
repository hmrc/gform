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

import cats.implicits._
import com.fasterxml.jackson.databind.JsonNode
import com.typesafe.config.{ ConfigFactory, ConfigRenderOptions }
import org.apache.commons.text.StringEscapeUtils
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.libs.json._
import play.api.mvc._
import uk.gov.hmrc.gform.BuildInfo
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.des.DesAlgebra
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.form.FormAlgebra
import uk.gov.hmrc.gform.formtemplate.{ BooleanExprId, BooleanExprSubstitutions, ExprSubstitutions, ExpressionId, FormTemplateAlgebra, RequestHandlerAlg, Substituter, TopLevelExpressions }
import uk.gov.hmrc.gform.repo.Repo
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.destinations.{ DataStoreSubmitter, DestinationSubmissionInfo, DestinationsProcessorModelAlgebra }
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.MongoComponent

import java.time.{ LocalDateTime, ZoneId }
import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.Try

class TestOnlyController(
  controllerComponents: ControllerComponents,
  mongoComponent: MongoComponent,
  enrolmentConnector: EnrolmentConnector,
  formAlgebra: FormAlgebra[Future],
  formTemplateAlgebra: FormTemplateAlgebra[Future],
  destinationsModelProcessorAlgebra: DestinationsProcessorModelAlgebra[Future],
  dataStoreSubmitter: DataStoreSubmitter,
  des: DesAlgebra[Future],
  testOnlyFormService: TestOnlyFormService,
  handler: RequestHandlerAlg[FOpt]
)(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {
  private val logger = LoggerFactory.getLogger(getClass)

  private val formTemplatesRepo = new Repo[FormTemplate]("formTemplate", mongoComponent, _._id.value)

  private def substituteExpr(exprSubstitutions: ExprSubstitutions): ExprSubstitutions = {
    import uk.gov.hmrc.gform.formtemplate.ExprSubstituter._
    val substituter = implicitly[Substituter[ExprSubstitutions, Expr]]
    ExprSubstitutions(exprSubstitutions.expressions.map { case (id, expr) =>
      id -> substituter.substitute(exprSubstitutions, expr)
    })
  }

  private def substituteBooleanExprsInExprs(
    booleanExprSubstitutions: BooleanExprSubstitutions,
    exprSubstitutions: ExprSubstitutions
  ): (ExprSubstitutions, Map[ExpressionId, String]) = {
    import uk.gov.hmrc.gform.formtemplate.BooleanExprSubstituter._
    val substituter = implicitly[Substituter[BooleanExprSubstitutions, Expr]]
    val (errors, successes) = exprSubstitutions.expressions.partitionMap { case (id, expr) =>
      Either
        .catchOnly[NoSuchElementException](substituter.substitute(booleanExprSubstitutions, expr))
        .fold(
          error => Left(id -> error.getMessage),
          success => Right(id -> success)
        )
    }
    (ExprSubstitutions(successes.toMap), errors.toMap)
  }

  private def substituteBooleanExpr(
    booleanExprSubstitutions: BooleanExprSubstitutions,
    exprSubstitutions: ExprSubstitutions
  ): Map[BooleanExprId, BooleanExpr] = {
    import uk.gov.hmrc.gform.formtemplate.BooleanExprSubstituter._

    val substituter = implicitly[Substituter[BooleanExprSubstitutions, BooleanExpr]]
    import uk.gov.hmrc.gform.formtemplate.Substituter.SubstituterSyntax
    import uk.gov.hmrc.gform.formtemplate.ExprSubstituter.booleanExprSubstituter

    val booleanExprs = booleanExprSubstitutions.expressions.map { case (id, beExpr) =>
      id -> beExpr(exprSubstitutions)
    }

    booleanExprs.map { case (id, expr) =>
      id -> substituter.substitute(booleanExprSubstitutions, expr)
    }
  }

  def getExpressions(formTemplateId: FormTemplateRawId): Action[AnyContent] =
    Action.async { _ =>
      testOnlyFormService
        .getFormTemplateRaw(formTemplateId)
        .map { rawTemplate =>
          val exprs = for {
            expressionsContext        <- ExprSubstitutions.from(rawTemplate)
            booleanExpressionsContext <- BooleanExprSubstitutions.from(rawTemplate)
          } yield {
            val expressions0: ExprSubstitutions = substituteExpr(expressionsContext)
            val expressionsSubsWithErrorMap: (ExprSubstitutions, Map[ExpressionId, String]) =
              substituteBooleanExprsInExprs(booleanExpressionsContext, expressions0)
            val expressionsResolved: Either[UnexpectedState, ExprSubstitutions] =
              TopLevelExpressions.resolveReferences(expressionsSubsWithErrorMap._1)
            ExpressionsLookup(
              expressionsResolved match {
                case Right(resolved) => resolved.expressions
                case Left(_)         => expressionsSubsWithErrorMap._1.expressions
              },
              substituteBooleanExpr(booleanExpressionsContext, expressionsSubsWithErrorMap._1),
              expressionsSubsWithErrorMap._2
            )
          }

          exprs match {
            case Right(el) => Ok(Json.toJson(el))
            case Left(e)   => BadRequest(e.error)
          }
        }
    }

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
                     submissionData.structuredFormData
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
                     submissionData.structuredFormData
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
              case h: Destination.HmrcDms =>
                h.payload match {
                  case None => BadRequest(s"Destination '${h.id.id}' is missing payload data")
                  case Some(payload) =>
                    val res = RealHandlebarsTemplateProcessor(
                      payload,
                      HandlebarsTemplateProcessorModel.empty,
                      FocussedHandlebarsModelTree(modelTree),
                      h.payloadType
                    )
                    Ok(res)
                }
              case dataStore: Destination.DataStore =>
                val submissionInfo: DestinationSubmissionInfo = DestinationSubmissionInfo(customerIdHeader, submission)
                val structuredFormData: StructuredFormValue.ObjectStructure = submissionData.structuredFormData
                val userSession: UserSession = submissionData.userSession
                val taxpayerId: Option[String] = destinationResult.flatMap(_.taxpayerId)
                val accumulatedModel: HandlebarsTemplateProcessorModel = HandlebarsTemplateProcessorModel.empty

                val tryResult = Try {
                  val payload = dataStoreSubmitter.generatePayload(
                    submissionInfo,
                    structuredFormData,
                    dataStore,
                    LangADT.En,
                    userSession,
                    taxpayerId,
                    accumulatedModel,
                    modelTree
                  )
                  dataStoreSubmitter.validateSchema(
                    dataStore,
                    payload
                  ) match {
                    case Left(message) => s"$message \n ${Json.prettyPrint(Json.parse(payload))}"
                    case _             => payload
                  }
                }

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
          case (d: Destination.HmrcDms) if d.id === id           => true
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
    Results.Ok(Json.toJson("version: " + BuildInfo.version))
  }

  private def flattenExceptionMessage(ex: Throwable): List[String] = {
    @tailrec
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

    implicit val format: OFormat[User] = OFormat[User](reads, write)
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
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val saveRequest: SaveRequest = request.body
      testOnlyFormService.saveForm(saveRequest).map(saveReply => Ok(Json.toJson(saveReply)))
    }

  def restoreForm(snapshotId: String) = Action.async { _ =>
    implicit val hc: HeaderCarrier = HeaderCarrier()
    testOnlyFormService
      .restoreForm(SnapshotId(snapshotId))
      .map(snapshot => Ok(Json.toJson(snapshot)))
  }

  def restoreFormWithAccessCode(snapshotId: String, accessCode: String) = Action.async { _ =>
    implicit val hc: HeaderCarrier = HeaderCarrier()
    testOnlyFormService
      .restoreFormWithAccessCode(SnapshotId(snapshotId), accessCode)
      .map(snapshot => Ok(Json.toJson(snapshot)))
  }

  def restoreSnapshotTemplate() = Action.async(parse.json) { request =>
    val snapshotId = request.body.as[SnapshotId]
    testOnlyFormService.restoreSnapshotTemplate(snapshotId).map(_ => Ok)
  }

  def getSnapshots() =
    Action.async(parse.json[SnapshotFilter]) { request =>
      val snapshotFilter: SnapshotFilter = request.body
      testOnlyFormService.getSnapshots(snapshotFilter).map(snapshots => Ok(Json.toJson(snapshots)))
    }

  def getSnapshotData(snapshotId: SnapshotId) = Action.async { _ =>
    testOnlyFormService
      .getSnapshotData(snapshotId)
      .map(snapshotWithData => Ok(Json.toJson(snapshotWithData)))
  }

  def updateSnapshot() =
    Action.async(parse.json[UpdateSnapshotRequest]) { request =>
      val updateRequest: UpdateSnapshotRequest = request.body
      testOnlyFormService.updateSnapshot(updateRequest).map(saveReply => Ok(Json.toJson(saveReply)))
    }

  def updateFormData() =
    Action.async(parse.json[UpdateFormDataRequest]) { request =>
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val updateRequest: UpdateFormDataRequest = request.body
      testOnlyFormService.updateFormData(updateRequest).map(saveReply => Ok(Json.toJson(saveReply)))
    }

  def deleteSnapshot(snapshotId: SnapshotId) = Action.async { _ =>
    testOnlyFormService
      .deleteSnapshot(snapshotId)
      .map(_ => NoContent)
  }

  def deleteGeneratedFiles(envelopeId: EnvelopeId): Action[AnyContent] = Action.async { implicit request =>
    testOnlyFormService
      .deleteGeneratedFiles(envelopeId)
      .fold(
        _ => throw new Exception(s"Unable to delete the generated files for the envelope '${envelopeId.value}'"),
        _ => NoContent
      )
  }

  def reloadTemplates(): Future[Unit] =
    for {
      formTemplateRaws <- testOnlyFormService.getAllFormTemplates()
      _ <- formTemplateRaws.traverse { formTemplateRaw =>
             handler
               .handleRequest(formTemplateRaw)
               .fold(
                 err => throw new Exception(s"Unable to reload the form templates. Error, $err"),
                 _ => {
                   logger.info(s"reloading the form template. handler.handleRequest(${formTemplateRaw._id.value})")
                   ()
                 }
               )
           }
    } yield ()
}

final case class RenderableDestination(
  id: DestinationId,
  payload: Option[String],
  payloadType: TemplateType
)
