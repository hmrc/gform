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

package uk.gov.hmrc.gform.sdes

import org.slf4j.LoggerFactory
import play.api.http.Status
import play.api.libs.json.{ JsError, JsResult, JsSuccess, JsValue, Json }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesNotifyRequest
import uk.gov.hmrc.gform.wshttp.{ FutureHttpResponseSyntax, WSHttp }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import scala.concurrent.{ ExecutionContext, Future }

class SdesConnector(
  wSHttp: WSHttp,
  sdesBaseUrl: String,
  sdesBasePath: String,
  sdesKeyAndCredentialsApiBaseUrl: String,
  sdesKeyAndCredentialsApiBasePath: String
)(implicit
  ex: ExecutionContext
) {
  private val logger = LoggerFactory.getLogger(getClass)

  private def mkHeaders(sdesRouting: SdesRouting): Seq[(String, String)] = Seq(
    "x-client-id"  -> sdesRouting.apiKey,
    "Content-Type" -> "application/json"
  )

  def notifySDES(payload: SdesNotifyRequest, sdesRouting: SdesRouting)(implicit
    hc: HeaderCarrier
  ): Future[HttpResponse] = {
    logger.info(s"SDES notification request: ${Json.stringify(Json.toJson(payload))}")
    val headers = mkHeaders(sdesRouting)
    val url = s"$sdesBaseUrl$sdesBasePath/notification/fileready"
    wSHttp
      .POST[SdesNotifyRequest, HttpResponse](url, payload, headers)
      .failWithNonSuccessStatusCodes(url)
  }

  def getPublicKey()(implicit hc: HeaderCarrier): Future[SdesPublicKey] = {
    val identifier = "sdes-encryption-public-key"
    logger.info(s"Calling $identifier api")
    val url = s"$sdesKeyAndCredentialsApiBaseUrl$sdesKeyAndCredentialsApiBasePath/sdes-encryption-public-key/current"
    wSHttp
      .GET[HttpResponse](url)
      .map { httpResponse =>
        httpResponse.status match {
          case Status.OK =>
            processPublicKey(httpResponse.json).fold(
              invalid =>
                throw new RuntimeException(
                  s"Calling $identifier returned successfully, but marshalling of data failed with: $invalid"
                ),
              valid => {
                logger.info(s"Calling $identifier returned Success.")
                valid
              }
            )
          case other =>
            throw new RuntimeException(
              s"Problem when calling $identifier. Http status: $other, body: ${httpResponse.body}"
            )
        }
      }
  }

  private def processPublicKey(json: JsValue): JsResult[SdesPublicKey] =
    json.validate[SdesPublicKey] match {
      case JsSuccess(publicKey, _) => JsSuccess(publicKey)
      case unexpected              => JsError(s"Expected object response for $unexpected")
    }
}
