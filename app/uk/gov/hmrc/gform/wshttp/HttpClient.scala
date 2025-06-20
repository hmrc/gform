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

package uk.gov.hmrc.gform.wshttp

import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.instances.all._
import cats.{ Endo, MonadError }
import com.typesafe.config.Config
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import java.net.URL
import scala.concurrent.ExecutionContext

trait HttpClient[F[_]] {
  def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse]

  def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse]

  def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse]
}

object HttpClient {

  private val logger = LoggerFactory.getLogger(getClass)

  def unimplementedHttpClient[F[_]]: HttpClient[F] = new HttpClient[F] {
    override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
    override def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
    override def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
  }

  implicit class HttpClientBuildingSyntax[F[_]](underlying: HttpClient[F]) {
    def buildUri(uriBuilder: Endo[String]): UriBuildingHttpClient[F] = new UriBuildingHttpClient(uriBuilder, underlying)

    def buildHeaderCarrier(headerCarrierBuilder: Endo[HeaderCarrier]): HeaderCarrierBuildingHttpClient[F] =
      new HeaderCarrierBuildingHttpClient(headerCarrierBuilder, underlying)

    def successResponsesOnly(implicit monadError: MonadError[F, Throwable]): SuccessfulResponseHttpClient[F] =
      new SuccessfulResponseHttpClient(underlying)

    def json(implicit monadError: MonadError[F, Throwable]): HttpClient[F] = new HttpClient[F] {
      private val contentType = "application/json"

      override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = underlying.get(uri)

      def post(uri: String, jsonString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.post(_, _)(addContentTypeHeader(hc, contentType)), uri, jsonString)(Some(Json.parse))

      def put(uri: String, jsonString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.put(_, _)(addContentTypeHeader(hc, contentType)), uri, jsonString)(Some(Json.parse))
    }

    def xml(implicit monadError: MonadError[F, Throwable]): HttpClient[F] = new HttpClient[F] {
      private val contentType = "application/xml"

      override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        underlying.get(uri)

      def post(uri: String, xmlString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.post(_, _)(addContentTypeHeader(hc, contentType)), uri, xmlString)(None)

      def put(uri: String, xmlString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.put(_, _)(addContentTypeHeader(hc, contentType)), uri, xmlString)(None)
    }
  }

  private def addContentTypeHeader(hc: HeaderCarrier, contentType: String): HeaderCarrier =
    hc.copy(extraHeaders = ("Content-Type" -> contentType) :: hc.extraHeaders.toList)

  private def httpRequest[F[_]](method: (String, String) => F[HttpResponse], uri: String, payload: String)(
    parser: Option[String => JsValue]
  )(implicit monadError: MonadError[F, Throwable]): F[HttpResponse] =
    try method(uri, parser.fold(payload)(fn => fn(payload).toString))
    catch {
      case ex: Exception =>
        logger.debug(s"Failed to send request to $uri with body: $payload", ex)
        monadError.raiseError(new Exception(s"Attempt send a request failed because the given body is not valid.", ex))
    }
}

class UriBuildingHttpClient[F[_]](uriBuilder: Endo[String], underlying: HttpClient[F]) extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = underlying.get(uriBuilder(uri))
  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.post(uriBuilder(uri), body)
  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.put(uriBuilder(uri), body)
}

class HeaderCarrierBuildingHttpClient[F[_]](headerCarrierBuilder: Endo[HeaderCarrier], underlying: HttpClient[F])
    extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.get(uri)(headerCarrierBuilder(hc))

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.post(uri, body)(headerCarrierBuilder(hc))

  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.put(uri, body)(headerCarrierBuilder(hc))
}

class SuccessfulResponseHttpClient[F[_]](underlying: HttpClient[F])(implicit monadError: MonadError[F, Throwable])
    extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    handleStatus(underlying.get(uri), "GET", uri)

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    handleStatus(underlying.post(uri, body), "POST", uri)

  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    handleStatus(underlying.put(uri, body), "PUT", uri)

  private def handleStatus(response: F[HttpResponse], method: String, uri: String): F[HttpResponse] =
    response.flatMap { r =>
      if (r.isSuccess) r.pure[F]
      else monadError.raiseError(new Exception(SuccessfulResponseHttpClient.unsuccessfulMessage(method, uri, r.status)))
    }
}

object SuccessfulResponseHttpClient {
  def unsuccessfulMessage(method: String, uri: String, statusCode: Int): String =
    s"Couldn't $method from URI '$uri'. Got response status code $statusCode"
}

class AuditingHttpClient(wsHttp: WSHttp, config: Config)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {

  private val logger = LoggerFactory.getLogger(getClass)

  val internalHostPatterns = HeaderCarrier.Config.fromConfig(config).internalHostPatterns

  private def isInternalCall(uri: String): Boolean =
    internalHostPatterns.exists(x => x.pattern.matcher(new URL(uri).getHost).matches())

  private def authHeaders(uri: String)(implicit hc: HeaderCarrier): Seq[(String, String)] =
    if (isInternalCall(uri)) {
      Seq.empty //No extra header for calls inside MDTP
    } else {
      hc.authorization.map(authToken => Seq("Authorization" -> authToken.value)).getOrElse(Seq.empty)
    }

  override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(
    wsHttp.GET(uri, headers = authHeaders(uri)).map(handleResponse("GET", uri))
  )

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.POSTString[HttpResponse](uri, body, headers = authHeaders(uri))).map(handleResponse("POST", uri))

  // TODO: Lance - when my pull request is merged, change this to use PUTString
  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.PUT(uri, Json.parse(body), headers = authHeaders(uri))).map(handleResponse("PUT", uri))

  private def is2xx(status: Int) = status >= 200 && status < 300

  def handleResponse(httpMethod: String, uri: String)(response: HttpResponse): HttpResponse =
    if (is2xx(response.status)) {
      response
    } else {
      logger.error(s"Sending $httpMethod to $uri, response status: ${response.status}, body: ${response.body}")
      response
    }
}

class WSHttpHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {
  override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(wsHttp.doGet(uri))

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.doPostString(uri, body, Seq.empty))

  // TODO: Lance - when my pull request is merged, change this to use doPutString
  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.doPut(uri, Json.parse(body)))
}
