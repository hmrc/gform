/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.{ Endo, MonadError }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.core.{ FOpt, _ }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import scala.concurrent.ExecutionContext

trait HttpClient[F[_]] {
  def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse]

  def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse]

  def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse]
}

object HttpClient {
  def wsHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext): HttpClient[FOpt] =
    new WSHttpHttpClient(wsHttp: WSHttp)

  def auditingwsHttpClient[F[_]](wsHttp: WSHttp)(implicit ec: ExecutionContext): HttpClient[FOpt] =
    new AuditingHttpClient(wsHttp: WSHttp)

  def unimplementedHttpClient[F[_]]: HttpClient[F] = new HttpClient[F] {
    override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
    override def post(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
    override def put(uri: String, body: String)(implicit hc: HeaderCarrier): F[HttpResponse] = ???
  }

  implicit class HttpClientBuildingSyntax[F[_]](underlying: HttpClient[F]) {
    def buildUri(uriBuilder: Endo[String]): UriBuildingHttpClient[F] = new UriBuildingHttpClient(uriBuilder, underlying)

    def buildHeaderCarrier(headerCarrierBuilder: Endo[HeaderCarrier]): HeaderCarrierBuildingHttpClient[F] =
      new HeaderCarrierBuildingHttpClient(headerCarrierBuilder, underlying)

    def successResponsesOnly(implicit monadError: MonadError[F, String]): SuccessfulResponseHttpClient[F] =
      new SuccessfulResponseHttpClient(underlying)

    def json(implicit monadError: MonadError[F, String]): HttpClient[F] = new HttpClient[F] {
      private val contentType = "application/json"

      override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = underlying.get(uri)

      def post(uri: String, jsonString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.post(_, _)(addContentTypeHeader(hc, contentType)), uri, jsonString)(Some(Json.parse))

      def put(uri: String, jsonString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.put(_, _)(addContentTypeHeader(hc, contentType)), uri, jsonString)(Some(Json.parse))
    }

    def soapXml(implicit monadError: MonadError[F, String]): HttpClient[F] = new HttpClient[F] {
      private val contentType = "application/soap+xml; charset=utf-8"

      override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = underlying.get(uri)

      def post(uri: String, xmlString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.post(_, _)(addContentTypeHeader(hc, contentType)), uri, xmlString)(None)

      def put(uri: String, xmlString: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
        httpRequest(underlying.put(_, _)(addContentTypeHeader(hc, contentType)), uri, xmlString)(None)
    }
  }

  private def addContentTypeHeader(hc: HeaderCarrier, contentType: String): HeaderCarrier =
    hc.copy(extraHeaders = ("Content-Type" -> contentType) :: hc.extraHeaders.toList)

  private def httpRequest[F[_]](method: (String, String) => F[HttpResponse], uri: String, payload: String)(
    parser: Option[String => JsValue])(implicit hc: HeaderCarrier, monadError: MonadError[F, String]): F[HttpResponse] =
    try {
      method(uri, parser.fold(payload)(fn => fn(payload).toString))
    } catch {
      case ex: Exception =>
        monadError.raiseError(
          s"Attempt send a request failed because the given String is not valid: ${ex.getMessage}. The String is: $payload")
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

class SuccessfulResponseHttpClient[F[_]](underlying: HttpClient[F])(implicit monadError: MonadError[F, String])
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
      else monadError.raiseError(SuccessfulResponseHttpClient.unsuccessfulMessage(method, uri, r.status))
    }
}

object SuccessfulResponseHttpClient {
  def unsuccessfulMessage(method: String, uri: String, statusCode: Int): String =
    s"Couldn't $method from URI '$uri'. Got response status code $statusCode"
}

class AuditingHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {
  private implicit val httpReads: HttpReads[HttpResponse] = new HttpReads[HttpResponse] {
    override def read(method: String, url: String, response: HttpResponse): HttpResponse = response
  }

  override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(wsHttp.GET(uri))

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.POSTString[HttpResponse](uri, body))

  // TODO: Lance - when my pull request is merged, change this to use PUTString
  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.PUT(uri, Json.parse(body)))
}

class WSHttpHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {
  override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(wsHttp.doGet(uri))

  override def post(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.doPostString(uri, body, Seq.empty))

  // TODO: Lance - when my pull request is merged, change this to use doPutString
  override def put(uri: String, body: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.doPut(uri, Json.parse(body)))
}
