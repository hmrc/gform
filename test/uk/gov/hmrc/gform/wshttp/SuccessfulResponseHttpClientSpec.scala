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

import cats.MonadError
import cats.syntax.either._
import org.scalacheck.Gen
import uk.gov.hmrc.gform.{ Possible, possibleMonadError }

class SuccessfulResponseHttpClientSpec extends HttpClientSpec {
  "get" should "delegate to underlying.get and return any response with a successful status" in httpClient[Possible] {
    underlying =>
      forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, response) =>
        whenever(response.isSuccess) {
          underlying.expectGet(uri, hc, response)

          buildClient(underlying.httpClient)
            .get(uri)(hc) shouldBe response.asRight
        }
      }
  }

  it should "delegate to underlying.get and then fail when the response with an successful status" in httpClient[
    Possible] { underlying =>
    forAll(Gen.alphaNumStr, headerCarrierGen, unsuccessfulHttpResponseGen) { (uri, hc, response) =>
      whenever(!response.isSuccess) {
        underlying.expectGet(uri, hc, response)

        buildClient(underlying.httpClient)
          .get(uri)(hc) shouldBe SuccessfulResponseHttpClient.unsuccessfulMessage("GET", uri, response.status).asLeft
      }
    }
  }

  "post" should "delegate to underlying.post and return any response with a successful status" in httpClient[Possible] {
    underlying =>
      forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) {
        (uri, postBody, hc, response) =>
          whenever(response.isSuccess) {
            underlying.expectPost(uri, postBody, hc, response)

            buildClient(underlying.httpClient)
              .post(uri, postBody)(hc) shouldBe response.asRight
          }
      }
  }

  it should "delegate to underlying.post and then fail when the response with an successful status" in httpClient[
    Possible] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, unsuccessfulHttpResponseGen) {
      (uri, postBody, hc, response) =>
        whenever(!response.isSuccess) {
          underlying.expectPost(uri, postBody, hc, response)

          buildClient(underlying.httpClient)
            .post(uri, postBody)(hc) shouldBe SuccessfulResponseHttpClient
            .unsuccessfulMessage("POST", uri, response.status)
            .asLeft
        }
    }
  }

  private def buildClient[F[_]](underlying: HttpClient[F])(implicit me: MonadError[F, String]): HttpClient[F] =
    underlying.successResponsesOnly
}
