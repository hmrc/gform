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

package uk.gov.hmrc.gform.envelope

import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import play.api.mvc.ControllerComponents
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import scala.concurrent.{ ExecutionContext, Future }

class EnvelopeController(
  cc: ControllerComponents,
  envelopeService: EnvelopeAlgebra[Future]
)(implicit ex: ExecutionContext)
    extends BaseController(cc) {
  private val logger = LoggerFactory.getLogger(getClass)

  def getEnvelope(envelopeId: EnvelopeId) = Action.async { _ =>
    logger.info(s"get envelope, envelopeId: '${envelopeId.value}'")
    envelopeService.find(envelopeId).map {
      case None           => NotFound
      case Some(envelope) => Ok(Json.toJson(envelope))
    }
  }

}
