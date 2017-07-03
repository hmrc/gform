/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.repository

import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import play.api.data
import play.api.libs.json.Json
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.repositories.FormRepository
import uk.gov.hmrc.mongo.MongoSpecSupport
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global

class FormRepositorySpec extends UnitSpec with MongoSpecSupport with BeforeAndAfterEach with ScalaFutures {

  val testForm = Form(
    FormId("testId"),
    FormData("12345", FormTypeId("TEST"), Version("0.2.0"), "TEST", Seq(FormField(FieldId("1"), "TEST")))
  )

  "Saving forms in db" should {
    "return successful" in {
      await(repo.insert(Json.obj("_id" -> "testId"), testForm))

      repo.findAll().futureValue.size should be(1)
    }
  }

  "Updating forms in db" should {
    "return successful" in {
      await(repo.insert(Json.obj("_id" -> "testId"), testForm))

      val updateForm = Form(
        FormId("testId"),
        FormData("12345", FormTypeId("TEST"), "0.2.0", "TEST", Seq(FormField(FieldId("1"), "UPDATETEST")))
      )

      repo.insert(Json.obj("_id" -> "testId"), testForm).futureValue

      repo.findAll().futureValue.size should be(1)
    }
  }

  val repo = new FormRepository()

  override protected def afterEach(): Unit = {
    repo.removeAll()
    super.afterEach()
  }

}
