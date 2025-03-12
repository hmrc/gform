/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.retrieval

import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status
import play.api.mvc.{ AnyContentAsEmpty, Result }
import play.api.test.FakeRequest
import play.api.test.Helpers.{ contentAsJson, contentAsString, defaultAwaitTimeout, status, stubControllerComponents }
import uk.gov.hmrc.auth.core.User
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.retrieval.AuthRetrievals

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthRetrievalControllerSpec
    extends AnyWordSpecLike with MockFactory with ScalaFutures with Matchers with ScalaCheckDrivenPropertyChecks {

  "upsertRetrievals" should {
    "return no content when upserting new auth retrievals" in new Fixture {
      (mockAuthRetrievalService
        .upsert(_: AuthRetrievals))
        .expects(retrievals)
        .returns(Future.successful(()))

      val future: Future[Result] = controller
        .upsertRetrievals()
        .apply(postRequest)

      val statusCode: Int = status(future)
      val bodyText: String = contentAsString(future)

      statusCode shouldBe Status.NO_CONTENT
      bodyText shouldBe empty
    }
  }

  "getRetrievals" should {
    "return valid auth retrievals" in new Fixture {
      (mockAuthRetrievalService
        .get(_: EnvelopeId))
        .expects(envelopeId)
        .returns(Future.successful(retrievals))

      val future: Future[Result] = controller
        .getRetrievals(envelopeId)
        .apply(getRequest)

      val statusCode: Int = status(future)
      val response: AuthRetrievals = contentAsJson(future).as[AuthRetrievals]

      statusCode shouldBe Status.OK
      response shouldBe retrievals
    }
  }

  trait Fixture {
    val mockAuthRetrievalService: AuthRetrievalService = mock[AuthRetrievalService]
    val controller: AuthRetrievalController =
      new AuthRetrievalController(mockAuthRetrievalService, stubControllerComponents())

    val envelopeId: EnvelopeId = EnvelopeId("some-env-id")
    val retrievals: AuthRetrievals = AuthRetrievals(
      envelopeId,
      Some("test@user.com"),
      emailLogin = false,
      ggLogin = true,
      Some("AB123456C"),
      None,
      Some("2223106666"),
      Some("123/FX987"),
      None,
      Some(AffinityGroup.Individual),
      Some(User)
    )

    val postRequest: FakeRequest[AuthRetrievals] = FakeRequest("POST", "/")
      .withBody(retrievals)

    val getRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
  }
}
