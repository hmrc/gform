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

import HttpClient.HttpClientBuildingSyntax
import cats.MonadError
import org.scalacheck.Gen
import uk.gov.hmrc.gform.{ Possible, possibleMonadError }

class JsonHttpClientSpec extends HttpClientSpec {
  "get" should "delegate to underlying.get with no changes" in httpClient[Possible] { underlying =>
    forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, response) =>
      underlying.expectGet(uri, hc, response)

      buildClient(underlying.httpClient)
        .get(uri)(hc) shouldBe Right(response)
    }
  }

  "postJsonString" should "delegate to underlying.post with no changes  when the JSON string is valid" in httpClient[
    Possible] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) {
      (uri, postBody, hc, response) =>
        underlying.expectPost(
          uri,
          s""""$postBody"""",
          hc.copy(extraHeaders = ("Content-Type" -> "application/json") :: hc.extraHeaders.toList),
          response)

        buildClient(underlying.httpClient)
          .post(uri, s""""$postBody"""")(hc) shouldBe Right(response)
    }
  }

  it should "not delegate to underlying.post and fail when the JSON string is not valid" in httpClient[Possible] {
    underlying =>
      forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, _) =>
        buildClient(underlying.httpClient)
          .post(uri, "fail")(hc) match {
          case Left(_)  => succeed
          case Right(_) => fail("Unexpected response")
        }
      }
  }

  "putJsonString" should "delegate to underlying.put with no changes  when the JSON string is valid" in httpClient[
    Possible] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) {
      (uri, putBody, hc, response) =>
        underlying.expectPut(
          uri,
          s""""$putBody"""",
          hc.copy(extraHeaders = ("Content-Type" -> "application/json") :: hc.extraHeaders.toList),
          response)

        buildClient(underlying.httpClient)
          .put(uri, s""""$putBody"""")(hc) shouldBe Right(response)
    }
  }

  it should "not delegate to underlying.put and fail when the JSON string is not valid" in httpClient[Possible] {
    underlying =>
      forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, _) =>
        buildClient(underlying.httpClient)
          .put(uri, "fail")(hc) match {
          case Left(_)  => succeed
          case Right(_) => fail("Unexpected response")
        }
      }
  }

  private def buildClient[F[_]](underlying: HttpClient[F])(implicit me: MonadError[F, String]): HttpClient[F] =
    underlying.json
}
