/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.integrationFramework

import org.slf4j.LoggerFactory
import play.api.libs.json.{ Format, JsError, JsObject, JsString, JsSuccess, Json }
import play.api.mvc._
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.ProfileName
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.RequestBuilder
import uk.gov.hmrc.http.{ HeaderCarrier, HttpException, StringContextOps }

import java.net.URL
import java.time.format.DateTimeFormatter
import java.time.{ ZoneOffset, ZonedDateTime }
import scala.concurrent.ExecutionContext

case class ManageEmailsResponse(primaryEmail: String, secondaryEmails: Seq[String])

object ManageEmailsResponse {
  implicit val format: Format[ManageEmailsResponse] = Json.format
}

class IfController(configModule: ConfigModule, wSHttpModule: WSHttpModule, cc: ControllerComponents)(implicit
  ec: ExecutionContext
) extends BaseController(cc) {

  private val logger = LoggerFactory.getLogger(getClass)

  private class IfRequest(fullUrl: URL, headers: Seq[(String, String)]) {
    def get(implicit hc: HeaderCarrier): RequestBuilder =
      wSHttpModule.httpClient.get(url"$fullUrl")(hc).setHeader(headers: _*)
    def post(implicit hc: HeaderCarrier): RequestBuilder =
      wSHttpModule.httpClient.post(url"$fullUrl")(hc).setHeader(headers: _*)
  }

  private def ifRequest(path: String, correlationId: String) = {
    val profileConfig = configModule.DestinationsServicesConfig()(
      ProfileName("cma")
    ) //TODO: Not sure if it's a good idea to pull config from "cma" profile
    val fullUrl = s"${profileConfig.baseUrl}$path"

    val headers = Seq(
      "Accept"           -> "application/json",
      "X-Forwarded-Host" -> "MDTP",
      "X-Correlation-Id" -> correlationId,
      "Date"             -> DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss 'UTC'").format(ZonedDateTime.now(ZoneOffset.UTC))
    ) ++ profileConfig.authorization.map(key => "Authorization" -> key.value)

    new IfRequest(url"$fullUrl", headers)
  }

  private def withCorrelationIdHeader[T](req: Request[_])(f: String => T) =
    req.headers
      .get("correlationId")
      .map(correlationId => f(correlationId))
      .getOrElse(throw new RuntimeException("Request didn't contain correlationId header"))

  def manageEmails(eori: String): Action[AnyContent] = Action.async { implicit req =>
    withCorrelationIdHeader(req) { correlationId =>
      ifRequest(s"/fta/manageemails/v1?eori=$eori", correlationId).get.execute
        .map { resp =>
          if (resp.status > 299) {
            logger.error(
              s"non 2xx response from IF manageemails. Response code: ${resp.status}. Body: ${resp.body}"
            )
          }

          val respTyped = resp.json.validate[ManageEmailsResponse]

          respTyped match {
            case JsSuccess(respTyped, path) =>
              val respSeq = respTyped.secondaryEmails.map { secondaryEmail =>
                JsObject(
                  Map(
                    "primaryEmail"   -> JsString(respTyped.primaryEmail),
                    "secondaryEmail" -> JsString(secondaryEmail)
                  )
                )
              }
              val transformedJsonResp = Json.toJson(respSeq)
              Status(resp.status)(transformedJsonResp).as("application/json")
            case JsError(errors) =>
              logger.error(
                s"IF server returned an unexpected type for manageEmails API call. Json validation errors: $errors"
              )
              Status(resp.status)(resp.body).as("application/json")
          }
        }
        .recover(standardErrors(correlationId))
    }
  }

  private def standardErrors(correlationId: String): PartialFunction[Throwable, Result] = {
    case e: HttpException =>
      logger.error(
        s"Connection with IF failed. Status: ${e.responseCode}. Message: ${e.message}. CorrelationId: $correlationId"
      )
      Status(e.responseCode)(e.message)
    case e =>
      logger.error(s"Connection with IF failed. Message: ${e.getMessage}. CorrelationId: $correlationId", e)
      ServiceUnavailable
  }
}
