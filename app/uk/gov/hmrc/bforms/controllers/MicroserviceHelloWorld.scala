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

import cats.implicits._
import play.api.mvc._
import uk.gov.hmrc.bforms.model.SaveAndRetrieve
import uk.gov.hmrc.bforms.repositories.{ SaveAndRetrieveRepository, TestRepository }
import uk.gov.hmrc.bforms.services.{ FileUploadService, RetrieveService }
import uk.gov.hmrc.bforms.typeclasses.{ FindOne, FusFeUrl, FusUrl, ServiceUrl }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.Future

class MicroserviceHelloWorld(
    testRepository: TestRepository,
    fileUploadeService: FileUploadService
)(
    implicit
    fusUrl: ServiceUrl[FusUrl],
    fusFeUrl: ServiceUrl[FusFeUrl],
    repository: SaveAndRetrieveRepository
) extends BaseController {

  def hackSubmit(registrationNumber: String) = Action.async { implicit request =>
    RetrieveService.retrieve(registrationNumber).flatMap {
      case Some(x) => fileUploadeService.createEnvelop(x.value).fold(
        error => error.toResult,
        response => Ok(response)
      )
      case None => Future.successful(Ok(s"No form with registration number $registrationNumber found."))
    }
  }
}
