/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.hip

import cats.implicits.catsSyntaxApplicativeId
import org.slf4j.LoggerFactory
import play.api.mvc._
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.sdes.CorrelationId
import uk.gov.hmrc.http.HttpException

import scala.concurrent.{ ExecutionContext, Future }

class HipController(
  hipService: HipServiceAlgebra[Future],
  controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  def validateNiClaimReference(nino: String, claimReference: String): Action[AnyContent] =
    Action.async { implicit request =>
      withCorrelationId("validateNiClaimReference") { correlationId =>
        hipService
          .validateNIClaimReference(nino, claimReference, correlationId)
          .asOkJson
          .recover(standardErrors)
      }
    }

  def getEmployments(nino: String, taxYear: Int): Action[AnyContent] =
    Action.async { implicit request =>
      withCorrelationId("getEmployments") { correlationId =>
        hipService
          .getEmployments(nino, taxYear, correlationId)
          .asOkJson
          .recover(standardErrors)
      }
    }

  private def withCorrelationId(
    identifier: String
  )(f: CorrelationId => Future[Result])(implicit request: Request[AnyContent]): Future[Result] = {
    val maybeCorrelationId: Option[CorrelationId] = request.headers.get("correlationId").map(CorrelationId(_))
    maybeCorrelationId.fold[Future[Result]] {
      logger.error(s"No correlationId header provided in call to $identifier")
      BadRequest.pure[Future]
    }(f(_))
  }

  private def standardErrors: PartialFunction[Throwable, Result] = {
    case e: HttpException => Status(e.responseCode)
    case _                => ServiceUnavailable
  }
}
