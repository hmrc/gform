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
import cats.Applicative
import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen
import uk.gov.hmrc.http.logging._
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, Token, UserId }

trait HttpClientSpec extends Spec {
  case class Underlying[F[_]](httpClient: HttpClient[F])(implicit A: Applicative[F]) {
    def expectGet(uri: String, hc: HeaderCarrier, response: HttpResponse): Underlying[F] = {
      (httpClient
        .get(_: String)(_: HeaderCarrier))
        .expects(uri, hc)
        .returning(A.pure(response))
      this
    }

    def expectPost(uri: String, body: String, hc: HeaderCarrier, response: HttpResponse): Underlying[F] = {
      (httpClient
        .post(_: String, _: String)(_: HeaderCarrier))
        .expects(uri, body, hc)
        .returning(A.pure(response))
      this
    }

    def expectPut(uri: String, body: String, hc: HeaderCarrier, response: HttpResponse): Underlying[F] = {
      (httpClient
        .put(_: String, _: String)(_: HeaderCarrier))
        .expects(uri, body, hc)
        .returning(A.pure(response))
      this
    }
  }

  def unsuccessfulHttpResponseGen: Gen[HttpResponse] =
    for {
      status         <- Gen.oneOf(Gen.chooseNum(100, 199), Gen.chooseNum(300, 599))
      responseString <- Gen.option(Gen.alphaNumStr)
    } yield HttpResponse(status, responseString = responseString)

  def successfulHttpResponseGen: Gen[HttpResponse] =
    for {
      status         <- Gen.chooseNum(200, 299)
      responseString <- Gen.option(Gen.alphaNumStr)
    } yield HttpResponse(status, responseString = responseString)

  def httpResponseGen: Gen[HttpResponse] =
    for {
      status         <- Gen.chooseNum(100, 599)
      responseString <- Gen.option(Gen.alphaNumStr)
    } yield HttpResponse(status, responseString = responseString)

  def authorizationGen: Gen[Authorization] = Gen.alphaNumStr.map(Authorization)
  def userIdGen: Gen[UserId] = Gen.alphaNumStr.map(UserId)
  def tokenGen: Gen[Token] = Gen.alphaNumStr.map(Token)
  def forwardedForGen: Gen[ForwardedFor] = Gen.alphaNumStr.map(ForwardedFor)
  def sessionIdGen: Gen[SessionId] = Gen.alphaNumStr.map(SessionId)
  def requestIdGen: Gen[RequestId] = Gen.alphaNumStr.map(RequestId)
  def requestChainGen: Gen[RequestChain] = Gen.alphaNumStr.map(RequestChain(_))
  def stringPairGen: Gen[(String, String)] =
    for {
      l <- Gen.alphaNumStr
      r <- Gen.alphaNumStr
    } yield (l, r)
  def stringPairSeqGen: Gen[Seq[(String, String)]] = PrimitiveGen.zeroOrMoreGen(stringPairGen)
  def akamaiReputationGen: Gen[AkamaiReputation] = Gen.alphaNumStr.map(AkamaiReputation)

  def headerCarrierGen: Gen[HeaderCarrier] =
    for {
      authorization    <- Gen.option(authorizationGen)
      userId           <- Gen.option(userIdGen)
      token            <- Gen.option(tokenGen)
      forwarded        <- Gen.option(forwardedForGen)
      sessionId        <- Gen.option(sessionIdGen)
      requestId        <- Gen.option(requestIdGen)
      requestChain     <- requestChainGen
      nsStamp          <- Gen.chooseNum(0L, Long.MaxValue)
      extraHeaders     <- stringPairSeqGen
      trueClientIp     <- Gen.option(Gen.alphaNumStr)
      trueClientPort   <- Gen.option(Gen.alphaNumStr)
      gaToken          <- Gen.option(Gen.alphaNumStr)
      gaUserId         <- Gen.option(Gen.alphaNumStr)
      deviceID         <- Gen.option(Gen.alphaNumStr)
      akamaiReputation <- Gen.option(akamaiReputationGen)
      otherHeaders     <- stringPairSeqGen
    } yield
      HeaderCarrier(
        authorization = authorization,
        userId = userId,
        token = token,
        forwarded = forwarded,
        sessionId = sessionId,
        requestId = requestId,
        requestChain = requestChain,
        nsStamp = nsStamp,
        extraHeaders = extraHeaders,
        trueClientIp = trueClientIp,
        trueClientPort = trueClientPort,
        gaToken = gaToken,
        gaUserId = gaUserId,
        deviceID = deviceID,
        akamaiReputation = akamaiReputation,
        otherHeaders = otherHeaders
      )

  def httpClient[F[_]: Applicative](f: Underlying[F] => Any): Any = {
    val httpClient = mock[HttpClient[F]]
    val underlying = Underlying(httpClient)
    f(underlying)
  }
}
