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
import cats.syntax.either._
import org.scalacheck.Gen

class JsonHttpClientSpec extends HttpClientSpec {
  type Possible[T] = Either[String, T]

  implicit val monadError: MonadError[Possible, String] = new MonadError[Possible, String] {
    override def raiseError[A](e: String): Possible[A] = Left(e)
    override def flatMap[A, B](fa: Possible[A])(f: A => Possible[B]): Possible[B] = fa.flatMap(f)
    override def pure[A](x: A): Possible[A] = Right(x)
    override def tailRecM[A, B](a: A)(f: A => Possible[Either[A, B]]): Possible[B] = ???
    override def handleErrorWith[A](fa: Possible[A])(f: String => Possible[A]): Possible[A] = ???
  }

  "get" should "delegate to underlying.get with no changes" in httpClient[Possible] { underlying =>
    forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, response) =>
      underlying.expectGet(uri, hc, response)

      buildClient(underlying.httpClient)
        .get(uri)(hc) shouldBe Right(response)
    }
  }

  "post" should "delegate to underlying.post with no changes" in httpClient[Possible] { underlying =>
    forAll(Gen.alphaNumStr, Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) {
      (uri, postBody, hc, response) =>
        underlying.expectPost(uri, postBody, hc, response)

        buildClient(underlying.httpClient)
          .post(uri, postBody)(hc) shouldBe Right(response)
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
          .postJsonString(uri, s""""$postBody"""")(hc) shouldBe Right(response)
    }
  }

  it should "not delegate to underlying.post and fail when the JSON string is not valid" in httpClient[Possible] {
    underlying =>
      forAll(Gen.alphaNumStr, headerCarrierGen, successfulHttpResponseGen) { (uri, hc, response) =>
        (buildClient(underlying.httpClient)
          .postJsonString(uri, "fail")(hc)) match {
          case Left(_)  => succeed
          case Right(_) => fail("Unexpected response")
        }
      }
  }

  private def buildClient[F[_]](underlying: HttpClient[F])(implicit me: MonadError[F, String]): JsonHttpClient[F] =
    underlying.json
}
