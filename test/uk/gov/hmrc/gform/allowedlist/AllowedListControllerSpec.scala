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

package uk.gov.hmrc.gform.allowedlist

import cats.data.EitherT
import cats.syntax.either._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ Matchers, WordSpecLike }
import play.api.http.Status
import play.api.libs.json.{ JsResultException, JsValue, Json }
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.gform.controllers.ErrResponse
import uk.gov.hmrc.gform.core.{ FOpt, UniqueIdGenerator }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.allowedlist.APIMTDVatNumber

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AllowedListControllerSpec
    extends WordSpecLike with MockFactory with ScalaFutures with Matchers with GeneratorDrivenPropertyChecks {

  val apiMTDVatNumberListGen = for {
    id   <- Gen.numStr
    list <- Gen.listOf(APIMTDVatNumber(id))
  } yield list

  trait Fixture {
    val mockMTDVatNumberRepo = mock[RepoAlgebra[MTDVatNumber, FOpt]]
    implicit val uniqueIdGenerator = new UniqueIdGenerator {
      override def generate: String = "some-unique-id"
    }
    val controller = new AllowedListController(mockMTDVatNumberRepo, stubControllerComponents())
    def request(request: JsValue) =
      FakeRequest("PUT", "/")
        .withBody(request)
    def initMocks(
      apiMTDVatNumberList: List[APIMTDVatNumber] = List.empty,
      response: FOpt[Unit] = EitherT(Future.successful(().asRight[UnexpectedState]))) =
      (mockMTDVatNumberRepo
        .upsertBulk(_: Seq[MTDVatNumber]))
        .expects(apiMTDVatNumberList.map(_.toMTDVatNumber))
        .returns(response)
  }

  "add for mtdVatNumber" should {

    "bulk upsert mtdVatNumber entries" in new Fixture {
      forAll(apiMTDVatNumberListGen) { apiMTDVatNumberList =>
        initMocks(apiMTDVatNumberList = apiMTDVatNumberList)

        val future: Future[Result] = controller
          .add(AllowedListName.mtdVatNumber)
          .apply(request(Json.toJson(apiMTDVatNumberList)))

        val statusCode = status(future)
        val bodyText: String = contentAsString(future)
        statusCode shouldBe Status.CREATED
        bodyText shouldBe empty
      }
    }

    "handle UnexpectedState error" in new Fixture {

      initMocks(response = EitherT(Future.successful(UnexpectedState("some-error").asLeft[Unit])))

      val future: Future[Result] = controller
        .add(AllowedListName.mtdVatNumber)
        .apply(request(Json.toJson(List[APIMTDVatNumber]())))

      val statusCode = status(future)
      val bodyText: String = contentAsString(future)

      statusCode shouldBe Status.INTERNAL_SERVER_ERROR
      Json.parse(bodyText).as[ErrResponse] shouldBe ErrResponse("some-error", None, uniqueIdGenerator.generate)
    }

    "handle request body parsing error by throwing JsResultException" in new Fixture {
      assertThrows[JsResultException] {
        controller
          .add(AllowedListName.mtdVatNumber)
          .apply(request(Json.parse("""[ { "id-missing": "" }]""")))
      }
    }
  }
}
