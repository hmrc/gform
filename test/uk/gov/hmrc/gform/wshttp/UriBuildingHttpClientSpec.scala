/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.Id
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class UriBuildingHttpClientSpec extends HttpClientSpec with ScalaCheckDrivenPropertyChecks {
  "get" should "delegate to underlying.get with the extended URL" in httpClient[Id] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, httpResponseGen) { (uri, uri2, hc, response) =>
      underlying.expectGet(uri2, hc, response)

      buildClient(underlying.httpClient, uri, uri2)
        .get(uri)(hc) shouldBe response
    }
  }

  "post" should "delegate to underlying.post with the extended URL" in httpClient[Id] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, httpResponseGen) {
      (uri, uri2, postBody, hc, response) =>
        underlying.expectPost(uri2, postBody, hc, response)

        buildClient(underlying.httpClient, uri, uri2)
          .post(uri, postBody)(hc) shouldBe response
    }
  }

  "put" should "delegate to underlying.put with the extended URL" in httpClient[Id] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, httpResponseGen) {
      (uri, uri2, putBody, hc, response) =>
        underlying.expectPut(uri2, putBody, hc, response)

        buildClient(underlying.httpClient, uri, uri2)
          .put(uri, putBody)(hc) shouldBe response
    }
  }

  private def buildClient[F[_]](underlying: HttpClient[F], expected: String, replacement: String): HttpClient[F] =
    underlying.buildUri(verifyAndSwap(_, expected, replacement))

  private def verifyAndSwap(actual: String, expected: String, replacement: String): String = {
    actual shouldBe expected
    replacement
  }
}
