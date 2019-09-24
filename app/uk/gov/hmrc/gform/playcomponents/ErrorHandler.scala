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

package uk.gov.hmrc.gform.playcomponents

import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.libs.json.Json
import play.api.mvc.Results._
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper
import uk.gov.hmrc.gform.controllers.ErrResponse
import uk.gov.hmrc.play.http._

import scala.concurrent.Future
import uk.gov.hmrc.http.{ HttpException, JsValidationException, NotFoundException, Upstream4xxResponse, Upstream5xxResponse }

class ErrorHandler(
  //  val auditConnector: AuditConnector,
  environment: Environment,
  configuration: Configuration,
  sourceMapper: Option[SourceMapper])
    extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, None) //    with JsonErrorHandling //    with ErrorAuditingSettings // TODO: auditConnector.sendEvent(dataEvent(code, unexpectedError, request)
    {

  override protected def onBadRequest(request: RequestHeader, message: String): Future[Result] = {
    val response = ErrResponse(message)
    Logger.logger.info(response.toString)
    Future.successful(BadRequest(Json.toJson(response)))
  }

  override protected def onForbidden(request: RequestHeader, message: String): Future[Result] = {
    val response = ErrResponse(message)
    Logger.logger.info(response.toString)
    Future.successful(Forbidden(Json.toJson(response)))
  }

  override protected def onNotFound(request: RequestHeader, message: String): Future[Result] = {
    //if (environment.mode == Mode.Dev) super.onNotFound(request, message) else { below code }
    val m = if (message.isEmpty) s"Resource not found: '${request.path}'" else message
    val response = ErrResponse(m)
    Logger.logger.info(response.toString)
    Future.successful(NotFound(Json.toJson(response)))
  }

  override protected def onOtherClientError(
    request: RequestHeader,
    statusCode: Int,
    message: String): Future[Result] = {
    val response = ErrResponse(message)
    Logger.logger.info(response.toString)
    Future.successful(Status(statusCode)(Json.toJson(response)))
  }

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = exception match {
    // format: OFF
     case e: Upstream4xxResponse               => onUpstream4xxResponse(e)
     case e: Upstream5xxResponse               => onUpstream5xxResponse(e)
     case e: NotFoundException                 => onNotFoundException(e)
     case e: java.util.NoSuchElementException  => onNotFoundException(e)
     case e: HttpException                     => onHttpException(e)
     case e: JsValidationException             => onJsValidationException(e)
     case e: Throwable                         => onOtherException(e)
    // format: ON
  }

  private def onHttpException(e: HttpException) = {
    val response = ErrResponse(e.getMessage)
    Logger.logger.error(response.toString, e)
    Future.successful(InternalServerError(Json.toJson(response)))
  }

  private def onUpstream4xxResponse(e: Upstream4xxResponse) = {
    val response = ErrResponse(s"Upstream4xx: ${e.getMessage}")
    Logger.logger.info(response.toString, e)
    Future.successful(BadRequest(Json.toJson(response)))
  }

  private def onUpstream5xxResponse(e: Upstream5xxResponse) = {
    val response = ErrResponse(s"Upstream5xx: ${e.getMessage}")
    Logger.logger.error(response.toString, e)
    Future.successful(InternalServerError(Json.toJson(response)))
  }

  private def onNotFoundException(e: Exception) = {
    val response = ErrResponse(s"${e.getMessage}")
    Logger.logger.error(response.toString, e)
    Future.successful(NotFound(Json.toJson(response)))
  }

  private def onJsValidationException(e: JsValidationException) = {

    //    implicit val writes: Writes[Seq[(JsPath, Seq[ValidationError])]] = {
    //      Writes.at(???)
    //      TODO: finish it
    //      ???
    //    }
    //    val details: JsValue = Json.toJson(e.errors)
    val temporaryDetails = Some(Json.obj("details" -> e.errors.toString))
    val response = ErrResponse("Invalid json", temporaryDetails)
    Logger.logger.info(response.toString, e)
    Future.successful(BadRequest(Json.toJson(response)))
  }

  private def onOtherException(e: Throwable) = {
    val response = ErrResponse(e.getMessage)
    Logger.logger.error(response.toString, e)
    Future.successful(InternalServerError(Json.toJson(response)))
  }
}
