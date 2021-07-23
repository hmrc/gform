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

package uk.gov.hmrc.gform.repo

import org.mongodb.scala.model.Filters.and
import org.mongodb.scala.model.{ Filters, IndexModel }
import org.mongodb.scala.model.Indexes.ascending
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpec, Matchers }
import play.api.libs.json.{ JsArray, JsNumber, JsString, Json, OFormat }
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import java.time.format.DateTimeFormatter
import java.time.{ Instant, LocalDateTime, ZoneOffset }
import scala.concurrent.ExecutionContext.Implicits.global

case class MyEntity(
  _id: String,
  num: Int,
  ref: String,
  parentRefs: List[String],
  createdDate: LocalDateTime,
  updatedInstant: Instant
)

object MyEntity {
  import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats.Implicits._
  implicit val format: OFormat[MyEntity] = Json.format[MyEntity]
}

class RepoSpec extends FlatSpec with Matchers with DefaultPlayMongoRepositorySupport[MyEntity] with ScalaFutures {

  private val DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  protected override def afterAll(): Unit = {
    mongoComponent.database.drop().toFuture().futureValue
    super.afterAll()
  }

  trait TestFixture {
    implicit val now: LocalDateTime = LocalDateTime.of(2020, 1, 1, 1, 1, 1, 0)
    val entity: MyEntity = buildEntity(1)
  }

  override lazy val repository =
    new Repo[MyEntity](
      "myEntity",
      mongoComponent,
      _._id,
      Seq(
        IndexModel(ascending("num")),
        IndexModel(ascending("ref")),
        IndexModel(ascending("parentRefs")),
        IndexModel(ascending("createdDate"))
      )
    )

  "findDocumentAsJson" should "find the entity by given id as JsValue" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    repository.findDocumentAsJson("id1").futureValue shouldBe Some(
      Json.parse(
        s"""
           |{
           | "_id":"id1",
           | "num":1,
           | "ref":"",
           | "parentRefs":[],
           | "createdDate":{
           |     "$$date":"${DATE_TIME_FORMAT.format(now)}"
           | },
           | "updatedInstant":{
           |   "$$date":"${DATE_TIME_FORMAT.format(now)}"
           | }
           |}
           |""".stripMargin.split("\\n").map(_.trim).mkString
      )
    )
  }

  "getDocumentAsJson" should "get entity by given id as JsValue" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    repository.getDocumentAsJson("id1").futureValue shouldBe
      Json.parse(s"""
                    |{
                    | "_id":"id1",
                    | "num":1,
                    | "ref":"",
                    | "parentRefs":[],
                    | "createdDate":{
                    |     "$$date":"${DATE_TIME_FORMAT.format(now)}"
                    | },
                    | "updatedInstant":{
                    |   "$$date":"${DATE_TIME_FORMAT.format(now)}"
                    | }
                    |}
                    |""".stripMargin.split("\\n").map(_.trim).mkString)
  }

  "find" should "find the entity by given id" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    repository.find("id1").futureValue shouldBe Some(entity)
  }

  "findAll" should "return all entities" in new TestFixture {
    val entities = Seq(buildEntity(1), buildEntity(2))
    repository.collection.insertMany(entities).toFuture().futureValue
    repository.findAll().futureValue shouldBe entities
  }

  "get" should "get entity by given id" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    repository.get("id1").futureValue shouldBe entity
  }

  it should "throw NoSuchElementException if entity does not exist" in new TestFixture {
    val error = repository.get("1").failed.futureValue
    error shouldBe a[NoSuchElementException]
    error.getMessage shouldBe "myEntity for given id: '1' not found"
  }

  "search" should "return entities matching the given criteria" in new TestFixture {
    val entities = Seq(buildEntity(1, "r1", List("p1")), buildEntity(2, "r2", List("p1")))
    repository.collection.insertMany(entities).toFuture().futureValue
    repository
      .search(
        and(
          Filters.notEqual("ref", "r1"),
          Filters.in("parentRefs", "p1")
        )
      )
      .futureValue shouldBe entities.tail
  }

  "page" should "retrieve subset of entities" in new TestFixture {
    val entities = (1 to 10).map(i => buildEntity(i, s"r$i", List(s"p$i")))
    repository.collection.insertMany(entities).toFuture().futureValue
    val pageResults =
      repository.page(Filters.gte("createdDate", now), Filters.equal("createdDate", -1), 0, 5).futureValue
    pageResults shouldBe entities.drop(5).reverse
  }

  "count" should "retrieve count of entities matching query" in new TestFixture {
    val entities = (1 to 10).map(i => buildEntity(i, s"r$i", List(s"p$i")))
    repository.collection.insertMany(entities).toFuture().futureValue
    val count = repository.count(Filters.gt("num", 5)).futureValue
    count shouldBe 5
  }

  "projection" should "return selected attributes from collection" in new TestFixture {
    val entities = (1 to 10).map(i => buildEntity(i, s"r$i", List(s"p$i")))
    repository.collection.insertMany(entities).toFuture().futureValue
    val result = repository.projection("_id", "num", "ref", "parentRefs", "createdDate").futureValue
    result shouldBe entities.map(e =>
      Json.obj(
        "_id"         -> JsString(e._id),
        "num"         -> JsNumber(e.num),
        "ref"         -> JsString(e.ref),
        "parentRefs"  -> JsArray(e.parentRefs.map(JsString)),
        "createdDate" -> Json.obj("$date" -> JsString(e.createdDate.format(DATE_TIME_FORMAT)))
      )
    )
  }

  "upsert" should "insert the entity if its missing" in new TestFixture {
    val result = repository.upsert(entity).value.futureValue
    result shouldBe Right(())
    repository.get(entity._id).futureValue shouldBe entity
  }

  it should "update the entity if it already exists" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    val entityUpdated = entity.copy(ref = "updated-ref")
    val result = repository.upsert(entityUpdated).value.futureValue
    result shouldBe Right(())
    repository.get(entity._id).futureValue shouldBe entityUpdated
  }

  "insertBulk" should "insert entities in bulk" in new TestFixture {
    val entities = (1 to 10).map(i => buildEntity(i, s"r$i", List(s"p$i")))
    val result = repository.insertBulk(entities).value.futureValue
    result shouldBe Right(())
    repository.findAll().futureValue shouldBe entities
  }

  "delete" should "remove the entity with given id" in new TestFixture {
    repository.collection.insertOne(entity).toFuture().futureValue
    assert(repository.findAll().futureValue == List(entity))

    val result = repository.delete(entity._id).value.futureValue
    result shouldBe Right(())
    repository.findAll().futureValue shouldBe List.empty
  }

  "deleteAll" should "remove all entities from the collection" in new TestFixture {
    val entities = (1 to 10).map(i => buildEntity(i, s"r$i", List(s"p$i")))
    repository.collection.insertMany(entities).toFuture().futureValue
    assert(repository.findAll().futureValue == entities)

    val result = repository.deleteAll().value.futureValue
    result shouldBe Right(())
    repository.findAll().futureValue shouldBe List.empty
  }

  def buildEntity(num: Int, ref: String = "", parentRefs: List[String] = List.empty)(implicit
    now: LocalDateTime
  ): MyEntity =
    MyEntity(s"id$num", num, ref, parentRefs, now, now.toInstant(ZoneOffset.UTC))
}
