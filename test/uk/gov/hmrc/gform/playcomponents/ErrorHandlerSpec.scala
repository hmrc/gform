/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.playcomponents

import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Matchers, WordSpecLike }
import play.api.libs.json.JsResultException
import play.api.libs.json.Json.toJson
import play.api.mvc.RequestHeader
import play.api.{ ConfigLoader, Configuration, Environment }
import play.api.libs.json._
import play.api.mvc.Results.BadRequest
import uk.gov.hmrc.gform.controllers.ErrResponse
import uk.gov.hmrc.gform.core.UniqueIdGenerator

class ErrorHandlerSpec extends WordSpecLike with MockFactory with Matchers with ScalaFutures {

  trait Fixture {
    val mockEnv = mock[Environment]
    val mockConfig = mock[Configuration]
    val mockRequestHeader = mock[RequestHeader]
    (mockConfig.getOptional[String](_: String)(_: ConfigLoader[String])).expects("play.editor", *).returns(None)
    implicit val uniqueIdGenerator: UniqueIdGenerator = new UniqueIdGenerator {
      override def generate: String = "some-unique-id"
    }
    val errorHandler = new ErrorHandler(mockEnv, mockConfig, None)
    val jsResultException = JsResultException(Seq(__ \ "id" -> Seq(JsonValidationError("some error"))))
  }

  "onServerError" when {
    "given JsResultException" should {
      "should resolve to BadRequest result" in new Fixture {
        whenReady(errorHandler.onServerError(mockRequestHeader, jsResultException)) { result =>
          result shouldBe BadRequest(
            toJson(
              ErrResponse(
                "Invalid json",
                Some(Json.obj("details" -> jsResultException.errors.toString)),
                uniqueIdGenerator.generate
              )
            )
          )
        }
      }
    }
  }
}
