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

package uk.gov.hmrc.gform.save4later

import akka.http.scaladsl.model.StatusCodes
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, Matchers }
import uk.gov.hmrc.gform.MongoComponentSupport
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FormMongoCacheSpec
    extends FlatSpecLike with Matchers with MongoComponentSupport with ExampleData with ScalaFutures
    with BeforeAndAfterEach with BeforeAndAfterAll {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  implicit val hc: HeaderCarrier = HeaderCarrier()

  override protected def afterAll(): Unit = {
    mongoComponent.database.drop().toFuture().futureValue
    ()
  }

  override protected def afterEach(): Unit = {
    mongoComponent.database.getCollection("forms").drop().toFuture().futureValue
    ()
  }

  val formMongoCache = new FormMongoCache(
    new MongoCacheRepository[String](
      mongoComponent,
      "forms",
      true,
      1.minute,
      new CurrentTimestampSupport(),
      SimpleCacheId
    )
  )

  "upsert" should "insert the form against the given formId" in {
    val result: Unit = formMongoCache.upsert(formId, form).futureValue
    result shouldBe a[Unit]
    formMongoCache.find(formId).futureValue shouldBe Some(form)
  }

  "find" should "find the form for the given formId" in {
    formMongoCache.upsert(formId, form).futureValue
    val result = formMongoCache.find(formId).futureValue
    result shouldBe Some(form)
  }

  it should "return None if form does not exist" in {
    val result = formMongoCache.find(formId).futureValue
    result shouldBe None
  }

  "get" should "return the form for the given formId" in {
    formMongoCache.upsert(formId, form).futureValue
    val result = formMongoCache.get(formId).futureValue
    result shouldBe form
  }

  it should "throw exception if form does not exist" in {
    val result = formMongoCache.get(formId).failed.futureValue
    result shouldBe UpstreamErrorResponse(
      s"Not found 'form' for the given id: '${formId.value}'",
      StatusCodes.NotFound.intValue
    )
  }

  "delete" should "remove the form with given id" in {
    formMongoCache.upsert(formId, form).futureValue
    assert(formMongoCache.find(formId).futureValue.isDefined)

    val result = formMongoCache.delete(formId).futureValue
    result shouldBe a[Unit]
    formMongoCache.find(formId).futureValue shouldBe None
  }

  it should "ignore delete if form not exists" in {
    val result = formMongoCache.delete(formId).futureValue
    result shouldBe a[Unit]
  }
}
