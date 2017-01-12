/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.controllers

import play.api.Logger
import play.api.mvc._
import scala.concurrent.Future
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.bforms.repositories.TestRepository
import uk.gov.hmrc.bforms.services.FileUploadService
import uk.gov.hmrc.bforms.typeclasses.{ FusUrl, FusFeUrl, ServiceUrl }

class MicroserviceHelloWorld(
    testRepository: TestRepository,
    fileUploadeService: FileUploadService
)(
    implicit
    fusUrl: ServiceUrl[FusUrl],
    fusFeUrl: ServiceUrl[FusFeUrl]
) extends BaseController {

  def hello = Action.async { implicit request =>
    fileUploadeService.createEnvelop.map(res => Ok(res))
  }

  def failed = Action.async { implicit request =>
    Future.failed(new JsValidationException("in controller", "validation-error", getClass, Seq()))
  }
}
