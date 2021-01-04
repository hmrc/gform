/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.libs.json.{ JsValue, Json }
import play.api.mvc.{ ControllerComponents, Request, Result }
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ ExecutionContext, Future }

class FUInterceptorController(
  controllerComponents: ControllerComponents,
  wSHttp: WSHttp,
  serviceConfig: ServicesConfig,
  proxy: Proxy)(implicit ec: ExecutionContext)
    extends BaseController(controllerComponents) {
  self =>

  def intercept(pathParam: String) = Action.async(parse.tolerantText) { (r: Request[String]) =>
    val path = s"/$pathParam"
    if (shouldIntercept(path)) respondWithPredefinedResponse(path)
    else {

      val response =
        proxy[String](
          baseUrl = originalFileUploadBaseUrl,
          path = path,
          inboundRequest = r,
          bodyTransformer = makeAllFilesScanned(_))
      response
    }
  }

  private def makeAllFilesScanned(body: String): String =
    //by default we replace all quarantined statueses to be cleaned
    //this is because on local machines we must rely on obsolete fileupload run in Service Manager
    //yep, this is not the state of the art ...
    body.replaceAllLiterally("QUARANTINED", "CLEANED")

  def setPredefinedResponse(pathParam: String) = Action(parse.json[JsValue]) { implicit r =>
    val path = s"/$pathParam"
    predefinedResponses = predefinedResponses.updated(path, r.body)
    Ok(Json.toJson(predefinedResponses))
  }

  def getPredefinedResponses = Action { _ =>
    Ok(Json.toJson(predefinedResponses))
  }

  private var predefinedResponses = Map[String, JsValue]()

  private def shouldIntercept(envelope: String): Boolean = predefinedResponses.get(envelope).isDefined
  private def respondWithPredefinedResponse(path: String): Future[Result] = Ok(predefinedResponses(path)).pure[Future]
  private lazy val originalFileUploadBaseUrl = serviceConfig.baseUrl("file-upload-interceptor")
}
