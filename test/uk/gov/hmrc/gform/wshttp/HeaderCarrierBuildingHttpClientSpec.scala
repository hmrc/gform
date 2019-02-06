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
import org.scalacheck.Gen
import uk.gov.hmrc.http.HeaderCarrier

class HeaderCarrierBuildingHttpClientSpec extends HttpClientSpec {
  "get" should "delegate to underlying.get with the modified HeaderCarrier" in httpClient { underlying =>
    forAll(Gen.alphaNumStr, headerCarrierGen, headerCarrierGen, httpResponseGen) { (url, hc, hc2, response) =>
      underlying.expectGet(url, hc2, response)

      buildClient(underlying.httpClient, hc, hc2)
        .get(url)(hc) shouldBe response
    }
  }

  "post" should "delegate to underlying.post with the extended URL" in httpClient { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, headerCarrierGen, httpResponseGen) {
      (url, postBody, hc, hc2, response) =>
        underlying.expectPost(url, postBody, hc2, response)

        buildClient(underlying.httpClient, hc, hc2)
          .post(url, postBody)(hc) shouldBe response
    }
  }

  private def buildClient[F[_]](
    underlying: HttpClient[F],
    expected: HeaderCarrier,
    replacement: HeaderCarrier): HttpClient[F] =
    underlying.buildHeaderCarrier(verifyAndSwap(_, expected, replacement))

  private def verifyAndSwap(
    actual: HeaderCarrier,
    expected: HeaderCarrier,
    replacement: HeaderCarrier): HeaderCarrier = {
    actual shouldBe expected
    replacement
  }
}
