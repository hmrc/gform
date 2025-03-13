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
import play.api.libs.json.JsString
import play.api.mvc.{ AnyContentAsEmpty, Result }
import play.api.test.FakeRequest
import play.api.test.Helpers.{ contentAsJson, contentAsString, defaultAwaitTimeout, status, stubControllerComponents }
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
      JsString("""
                 |{
                 |  "_id": {
                 |    "envelopeId": "f0a77c8c-86f6-48dd-9d9f-dbadd3a37869"
                 |  },
                 |  "materialisedRetrievals": {
                 |    "AuthenticatedRetrievals": {
                 |      "governmentGatewayId": {
                 |        "ggId": "2147447375173198"
                 |      },
                 |      "enrolments": {
                 |        "enrolments": [
                 |          {
                 |            "identifiers": [
                 |              {
                 |                "key": "UTR",
                 |                "value": "2376236723"
                 |              }
                 |            ],
                 |            "state": "Activated",
                 |            "enrolment": "IR-SA"
                 |          },
                 |          {
                 |            "identifiers": [
                 |              {
                 |                "key": "NINO",
                 |                "value": "AB123456C"
                 |              }
                 |            ],
                 |            "state": "Activated",
                 |            "enrolment": "HMRC-NI"
                 |          }
                 |        ]
                 |      },
                 |      "affinityGroup": "individual",
                 |      "groupIdentifier": "123456SAIND",
                 |      "maybeNino": {
                 |        "value": "AB123456C"
                 |      },
                 |      "otherRetrievals": {
                 |        "name": {
                 |          "name": "TestUser"
                 |        },
                 |        "email": "user@test.com"
                 |      },
                 |      "confidenceLevel": 200,
                 |      "credentialRole": "User"
                 |    }
                 |  }
                 |}
                 |""".stripMargin)
    )

    val postRequest: FakeRequest[AuthRetrievals] = FakeRequest("POST", "/")
      .withBody(retrievals)

    val getRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
  }
}
