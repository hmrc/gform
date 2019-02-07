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
import cats.{ Id, MonadError }
import org.scalacheck.Gen
import uk.gov.hmrc.http.HttpResponse

class SuccessfulResponseHttpClientSpec extends HttpClientSpec {
  implicit val monadError: MonadError[Id, String] = new MonadError[Id, String] {
    override def raiseError[A](e: String): Id[A] = throw new Exception(e)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    override def pure[A](x: A): Id[A] = x

    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
    override def handleErrorWith[A](fa: Id[A])(f: String => Id[A]): Id[A] = ???
    override def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ???
  }

  "get" should "delegate to underlying.get and return any response with 200 <= status <= 299" in httpClient[Id] {
    underlying =>
      forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, response) =>
        whenever(successful(response)) {
          underlying.expectGet(uri, hc, response)

          buildClient(underlying.httpClient)
            .get(uri)(hc) shouldBe response
        }
      }
  }

  "post" should "delegate to underlying.post and return any response with 200 <= status <= 299" in httpClient[Id] {
    underlying =>
      forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) {
        (uri, postBody, hc, response) =>
          whenever(successful(response)) {
            underlying.expectPost(uri, postBody, hc, response)

            buildClient(underlying.httpClient)
              .post(uri, postBody)(hc) shouldBe response
          }
      }
  }

  private def successful(response: HttpResponse) = response.status >= 200 && response.status < 300

  private def buildClient[F[_]](underlying: HttpClient[F])(implicit me: MonadError[F, String]): HttpClient[F] =
    underlying.successResponsesOnly
}
