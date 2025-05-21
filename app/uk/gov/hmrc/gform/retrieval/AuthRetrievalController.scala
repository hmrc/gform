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

package uk.gov.hmrc.gform.retrieval

import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, ControllerComponents }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.retrieval.{ AuthRetrievals, AuthRetrievalsByFormIdData }

import scala.concurrent.ExecutionContext

class AuthRetrievalController(
  retrievalService: AuthRetrievalService,
  controllerComponents: ControllerComponents
)(implicit ex: ExecutionContext)
    extends BaseController(controllerComponents) {

  def upsertRetrievals(): Action[AuthRetrievals] =
    Action.async(parse.json[AuthRetrievals]) { request =>
      val authRetrievals: AuthRetrievals = request.body
      retrievalService.upsert(authRetrievals).map { _ =>
        NoContent
      }
    }

  def upsertRetrievalsByFormIdData(): Action[AuthRetrievalsByFormIdData] =
    Action.async(parse.json[AuthRetrievalsByFormIdData]) { implicit request =>
      val authRetrievals: AuthRetrievalsByFormIdData = request.body
      retrievalService.upsertByFormIdData(authRetrievals).map { _ =>
        NoContent
      }
    }

  def getRetrievals(envelopeId: EnvelopeId): Action[AnyContent] =
    Action.async { _ =>
      retrievalService
        .get(envelopeId)
        .map(retrieval => Ok(Json.toJson(retrieval)))
    }
}
