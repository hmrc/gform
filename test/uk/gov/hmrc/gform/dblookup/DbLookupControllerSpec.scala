/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.dblookup

import cats.syntax.either._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Matchers, WordSpecLike }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.http.Status
import play.api.libs.json.{ JsResultException, JsValue, Json }
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{ contentAsString, status, stubControllerComponents, _ }
import uk.gov.hmrc.gform.controllers.ErrResponse
import uk.gov.hmrc.gform.core.UniqueIdGenerator
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.dblookup.{ CollectionName, DbLookupId }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DbLookupControllerSpec
    extends WordSpecLike with MockFactory with ScalaFutures with Matchers with ScalaCheckDrivenPropertyChecks {

  val dbLookupIdListGen = for {
    id   <- Gen.numStr
    list <- Gen.listOf(DbLookupId(id))
  } yield list

  trait Fixture {
    val mockDbLookupService = mock[DbLookupService]
    implicit val uniqueIdGenerator = new UniqueIdGenerator {
      override def generate: String = "some-unique-id"
    }
    val controller = new DbLookupController(stubControllerComponents(), mockDbLookupService)
    def request(request: JsValue) =
      FakeRequest("PUT", "/")
        .withBody(request)
    def initMocks(
      dbLookupIds: List[DbLookupId] = List.empty,
      response: Future[Either[UnexpectedState, Unit]] = Future.successful(().asRight[UnexpectedState])) =
      (mockDbLookupService
        .addMulti(_: Seq[DbLookupId], _: CollectionName))
        .expects(dbLookupIds, CollectionName("mtdVatNumber"))
        .returns(response)
  }

  "add for mtdVatNumber" should {

    "bulk upsert mtdVatNumber entries" in new Fixture {
      forAll(dbLookupIdListGen) { dbLookupIds =>
        initMocks(dbLookupIds = dbLookupIds)

        val future: Future[Result] = controller
          .add(CollectionName("mtdVatNumber"))
          .apply(request(Json.toJson(dbLookupIds)))

        val statusCode = status(future)
        val bodyText: String = contentAsString(future)
        statusCode shouldBe Status.CREATED
        bodyText shouldBe empty
      }
    }

    "handle UnexpectedState error" in new Fixture {

      initMocks(response = Future.successful(UnexpectedState("some-error").asLeft[Unit]))

      val future: Future[Result] = controller
        .add(CollectionName("mtdVatNumber"))
        .apply(request(Json.toJson(List[DbLookupId]())))

      val statusCode = status(future)
      val bodyText: String = contentAsString(future)

      statusCode shouldBe Status.INTERNAL_SERVER_ERROR
      Json.parse(bodyText).as[ErrResponse] shouldBe ErrResponse("some-error", None, uniqueIdGenerator.generate)
    }

    "handle request body parsing error by throwing JsResultException" in new Fixture {
      assertThrows[JsResultException] {
        controller
          .add(CollectionName("mtdVatNumber"))
          .apply(request(Json.parse("""[ { "id-missing": "" }]""")))
      }
    }
  }
}
