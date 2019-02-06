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
import uk.gov.hmrc.gform.core.{ FOpt, _ }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import scala.concurrent.ExecutionContext

trait HttpClient[F[_]] {
  def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse]
  def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse]
}

object HttpClient {
  def wsHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext): HttpClient[FOpt] =
    new WSHttpHttpClient(wsHttp: WSHttp)
  def auditingwsHttpClient[F[_]](wsHttp: WSHttp)(implicit ec: ExecutionContext): HttpClient[FOpt] =
    new AuditingHttpClient(wsHttp: WSHttp)

  implicit class HttpClientBuildingSyntax[F[_]](underlying: HttpClient[F]) {
    def buildUri(uriBuilder: Endo[String]) = new UriBuildingHttpClient(uriBuilder, underlying)
    def buildHeaderCarrier(headerCarrierBuilder: Endo[HeaderCarrier]) =
      new HeaderCarrierBuildingHttpClient(headerCarrierBuilder, underlying)
    def successResponsesOnly(implicit monadError: MonadError[F, String]) = new SuccessfulResponseHttpClient(underlying)
  }
}

class UriBuildingHttpClient[F[_]](uriBuilder: Endo[String], underlying: HttpClient[F]) extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = underlying.get(uriBuilder(uri))

  override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.postJson(uriBuilder(uri), json)
}

class HeaderCarrierBuildingHttpClient[F[_]](headerCarrierBuilder: Endo[HeaderCarrier], underlying: HttpClient[F])
    extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.get(uri)(headerCarrierBuilder(hc))

  override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    underlying.postJson(uri, json)(headerCarrierBuilder(hc))
}

class SuccessfulResponseHttpClient[F[_]](underlying: HttpClient[F])(implicit monadError: MonadError[F, String])
    extends HttpClient[F] {
  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    handleStatus(underlying.get(uri), "GET", uri)

  override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    handleStatus(underlying.get(uri), "POST", uri)

  private def handleStatus(response: F[HttpResponse], method: String, uri: String): F[HttpResponse] =
    response.flatMap { r =>
      if (r.status >= 200 && r.status < 300) r.pure[F]
      else monadError.raiseError(s"Couldn't $method $uri")
    }
}

class AuditingHttpClient[F[_]](wsHttp: WSHttp)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {
  private implicit val httpReads: HttpReads[HttpResponse] = new HttpReads[HttpResponse] {
    override def read(method: String, url: String, response: HttpResponse): HttpResponse = response
  }

  override def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(wsHttp.GET(uri))

  override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.POSTString[HttpResponse](uri, json, List(("Content-Type", "application/json"))))
}

class WSHttpHttpClient(wsHttp: WSHttp)(implicit ec: ExecutionContext) extends HttpClient[FOpt] {
  def get(uri: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] = fromFutureA(wsHttp.doGet(uri))

  def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): FOpt[HttpResponse] =
    fromFutureA(wsHttp.doPostString(uri, json, List(("Content-Type", "application/json"))))
}
