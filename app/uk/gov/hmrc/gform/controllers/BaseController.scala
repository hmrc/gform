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

package uk.gov.hmrc.gform.controllers

import java.util.UUID

import cats.data.EitherT
import cats.implicits._
import org.slf4j.{ LoggerFactory, MDC }
import play.api.libs.json._
import play.api.mvc.{ AbstractController, Action, AnyContent, BodyParser, ControllerComponents, Request, Result }
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormIdData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider
import uk.gov.hmrc.play.bootstrap.controller.WithJsonBody

import scala.concurrent.{ ExecutionContext, Future }
import io.circe.Printer

class BaseController(controllerComponents: ControllerComponents)(implicit ec: ExecutionContext)
    extends AbstractController(controllerComponents) with BackendHeaderCarrierProvider with WithJsonBody {

  private val logger = LoggerFactory.getLogger(getClass)

  private val printer = Printer.spaces2
    .copy(
      colonLeft = "",
      lrbracketsEmpty = ""
    )

  object O {
    def asOkJson[T: Writes](t: T): Result =
      Ok(Json.toJson(t))

    def asOkPrettyJson[T: Writes](t: T): Result = {
      val jsonAsString: String = Json.stringify(Json.toJson(t))
      io.circe.parser.parse(jsonAsString) match {
        case Left(e) => BadRequest(e.getMessage)
        case Right(json) =>
          val formatted = printer
            .print(json)
            .replaceAll("\\{\\s*\\}", "{}") // render empty object as '{}'

          Ok(formatted).as(ContentType.`application/json`.value)
      }
    }
  }

  implicit class FutureOps[T: Writes](f: Future[T]) {
    def asOkJson = f.map(t => O.asOkJson(t))
    def asOkPrettyJson = f.map(t => O.asOkPrettyJson(t))
  }

  implicit class FutureOps2(f: Future[_]) {
    def asNoContent = f.map(_ => NoContent)
  }

  private type LeftResult[T] = EitherT[Future, Result, T]

  def asRes[T](fa: Future[T]): LeftResult[T] =
    EitherT[Future, Result, T](fa.map(_.asRight))

  def asRes[E, T](fa: Future[Either[E, T]])(toLeftResult: E => Result): LeftResult[T] =
    EitherT[Future, E, T](fa).leftMap(toLeftResult)

  def asRes[T](a: T): LeftResult[T] = EitherT[Future, Result, T](Future.successful(a.asRight))

  def formAction[A](bodyParser: BodyParser[A], endPoint: String, formIdData: FormIdData)(
    block: Request[A] => Future[Result]
  ): Action[A] =
    formAction[A](bodyParser, endPoint, formIdData.toFormId)(block)

  def formAction[A](bodyParser: BodyParser[A], endPoint: String, formId: FormId)(
    block: Request[A] => Future[Result]
  ): Action[A] =
    controllerComponents.actionBuilder.async(bodyParser) { request =>
      addFormIdToMdc(formId)
      logger.info(s"${getClass.getSimpleName}.$endPoint, ${loggingHelpers.cleanHeaders(request.headers)}")
      block(request)
    }

  def formAction(endPoint: String, formIdData: FormIdData, additionalInfo: String*)(
    block: Request[AnyContent] => Future[Result]
  ): Action[AnyContent] =
    formAction(endPoint, formIdData.toFormId, additionalInfo: _*)(block)

  def formAction(endPoint: String, formId: FormId, additionalInfo: String*)(
    block: Request[AnyContent] => Future[Result]
  ): Action[AnyContent] =
    controllerComponents.actionBuilder.async { request =>
      addFormIdToMdc(formId)
      logger.info(s"${getClass.getSimpleName}.$endPoint, ${additionalInfo.toList.mkString(", ")}")
      block(request)
    }

  private def addFormIdToMdc(formId: FormId): Unit =
    MDC.put("FormId", formId.value)

  def formTemplateAction(endPoint: String, formTemplateId: FormTemplateId)(
    block: Request[AnyContent] => Future[Result]
  ): Action[AnyContent] =
    controllerComponents.actionBuilder.async { request =>
      addFormTemplateIdToMdc(formTemplateId)
      logger.info(s"${getClass.getSimpleName}.$endPoint")
      block(request)
    }

  protected def addFormTemplateIdToMdc(formTemplateId: FormTemplateId): Unit =
    MDC.put("FormTemplateId", formTemplateId.value)
}

case class ErrResponse(
  error: String,
  details: Option[JsValue] = None,
  occurrenceId: String = UUID.randomUUID().toString
)

object ErrResponse {
  implicit val format: OFormat[ErrResponse] = Json.format[ErrResponse]
}
