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

import cats.{ Endo, Monad }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import org.slf4j.Logger
import uk.gov.hmrc.gform.core.{ FOpt, _ }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import scala.concurrent.ExecutionContext

trait HttpClient[F[_]] {
  def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse]
  def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse]
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

class LoggingHttpClient[F[_]: Monad](logger: Logger, underlying: HttpClient[F]) extends HttpClient[F] {
  private def log(s: String, f: => F[HttpResponse]): F[HttpResponse] = {
    logger.info(s)
  }.pure[F].flatMap(_ => f)

  override def get(uri: String)(implicit hc: HeaderCarrier): F[HttpResponse] = log(s"GET: $uri", underlying.get(uri))
  override def postJson(uri: String, json: String)(implicit hc: HeaderCarrier): F[HttpResponse] =
    log(s"POST: $uri, $json", underlying.postJson(uri, json))
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
