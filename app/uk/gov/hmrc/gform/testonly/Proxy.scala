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

package uk.gov.hmrc.gform.testonly

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.http.HttpEntity.{ Streamed, Strict }
import play.api.libs.streams.Accumulator
import play.api.libs.ws.{ BodyWritable, WSClient, WSRequest, WSResponse }
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }

class Proxy(wsClient: WSClient, controllerComponents: ControllerComponents)(implicit ec: ExecutionContext) {

  /** This creates action which proxies incoming request to remote service.
    */
  def apply(remoteServiceBaseUrl: String)(path: String): Action[Source[ByteString, _]] =
    controllerComponents.actionBuilder.async(streamedBodyParser) { inboundRequest: Request[Source[ByteString, _]] =>
      for {
        outboundRequest              <- proxyRequest(s"$remoteServiceBaseUrl/$path", inboundRequest)
        streamedResponse: WSResponse <- outboundRequest.stream()
      } yield {
        val headersMap = streamedResponse.headers
        val contentLength = headersMap.get(contentLengthHeaderKey).flatMap(_.headOption.map(_.toLong))
        val contentType = headersMap.get(contentTypeHeaderKey).map(_.mkString(", "))
        Result(
          ResponseHeader(
            streamedResponse.status,
            streamedResponse.headers.mapValues(_.head).filter(filterOutContentHeaders).toMap
          ),
          Streamed(streamedResponse.bodyAsSource, contentLength, contentType)
        )
      }
    }

  def apply[A: BodyWritable](
    baseUrl: String,
    path: String,
    inboundRequest: Request[A],
    bodyTransformer: String => String = identity
  ): Future[Result] = {

    val outboundRequest = wsClient
      .url(s"$baseUrl$path")
      .withFollowRedirects(false)
      .withMethod(inboundRequest.method)
      .withHttpHeaders(processHeaders(inboundRequest.headers, extraHeaders = Nil): _*)
      .withQueryStringParameters(inboundRequest.queryString.mapValues(_.head).toSeq: _*)
      .withBody(inboundRequest.body)

    val response: Future[WSResponse] = outboundRequest.execute()

    response.map { response =>
      val transformedBody: ByteString = ByteString(bodyTransformer(response.body))
      Result(
        header = ResponseHeader(response.status, response.headers.view.mapValues(_.head).toMap),
        body = Strict(transformedBody, None),
        newSession = None,
        newFlash = None,
        newCookies = Seq.empty
      )
    }
  }

  private lazy val contentTypeHeaderKey = "Content-Type"
  private lazy val contentLengthHeaderKey = "Content-Length"
  private lazy val filterOutContentHeaders: ((String, String)) => Boolean = { case (key, _) =>
    !key.equalsIgnoreCase(contentTypeHeaderKey) && !key.equalsIgnoreCase(contentLengthHeaderKey)
  }

  private def proxyRequest(path: String, inboundRequest: Request[Source[ByteString, _]]): Future[WSRequest] =
    Future(
      wsClient
        .url(s"$path")
        .withFollowRedirects(false)
        .withMethod(inboundRequest.method)
        .withHttpHeaders(processHeaders(inboundRequest.headers, extraHeaders = Nil): _*)
        .withQueryStringParameters(inboundRequest.queryString.mapValues(_.head).toSeq: _*)
        .withBody(inboundRequest.body)
    )

  private def processHeaders(inboundHeaders: Headers, extraHeaders: Seq[(String, String)]): Seq[(String, String)] =
    inboundHeaders.toSimpleMap.filter(headerKeyValue => !headerKeyValue._1.equals("Host")).toSeq ++ extraHeaders

  private def streamedBodyParser: BodyParser[Source[ByteString, _]] = BodyParser { _ =>
    Accumulator.source[ByteString].map(Right.apply)
  }

}
