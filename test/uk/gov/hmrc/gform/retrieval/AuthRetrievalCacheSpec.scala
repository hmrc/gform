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

import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import play.api.libs.json.JsString
import uk.gov.hmrc.crypto.SymmetricCryptoFactory.{ aesCrypto, composeCrypto }
import uk.gov.hmrc.gform.MongoComponentSupport
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormGen
import uk.gov.hmrc.gform.sharedmodel.retrieval.AuthRetrievals
import uk.gov.hmrc.http.{ HeaderCarrier, UpstreamErrorResponse }
import uk.gov.hmrc.mongo.CurrentTimestampSupport
import uk.gov.hmrc.mongo.cache.CacheIdType.SimpleCacheId
import uk.gov.hmrc.mongo.cache.MongoCacheRepository

import scala.concurrent.ExecutionContext.Implicits.global

class AuthRetrievalCacheSpec
    extends AnyFlatSpecLike with Matchers with MongoComponentSupport with ScalaFutures with BeforeAndAfterEach
    with BeforeAndAfterAll with FormGen {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val envelopeId = envelopeIdGen.pureApply(Gen.Parameters.default, Seed(1))
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
  override protected def afterAll(): Unit = {
    mongoComponent.database.drop().toFuture().futureValue
    ()
  }

  override protected def afterEach(): Unit = {
    mongoComponent.database.getCollection("retrievals").drop().toFuture().futureValue
    ()
  }

  val authRetrievalMongoCache = new AuthRetrievalCache(
    new MongoCacheRepository[String](
      mongoComponent,
      "retrievals",
      true,
      1.minute,
      new CurrentTimestampSupport(),
      SimpleCacheId
    ),
    composeCrypto(
      currentCrypto = aesCrypto("fqpLDZ4sumDsekHkeEBlCA=="),
      previousDecrypters = List.empty[String].map(aesCrypto)
    )
  )

  "upsert" should "insert the retrieval against the given envelopeId" in {
    val result: Unit = authRetrievalMongoCache.upsert(retrievals).futureValue
    result shouldBe a[Unit]
    authRetrievalMongoCache.find(envelopeId).futureValue shouldBe Some(retrievals)
  }

  "find" should "find the retrievals for the given envelopeId" in {
    authRetrievalMongoCache.upsert(retrievals).futureValue
    val result = authRetrievalMongoCache.find(envelopeId).futureValue
    result shouldBe Some(retrievals)
  }

  it should "return None if retrievals does not exist" in {
    val result = authRetrievalMongoCache.find(envelopeId).futureValue
    result shouldBe None
  }

  "get" should "return the retrievals for the given envelopeId" in {
    authRetrievalMongoCache.upsert(retrievals).futureValue
    val result = authRetrievalMongoCache.get(envelopeId).futureValue
    result shouldBe retrievals
  }

  it should "throw exception if retrievals does not exist" in {
    val result = authRetrievalMongoCache.get(envelopeId).failed.futureValue
    result shouldBe UpstreamErrorResponse(
      s"Not found 'authRetrieval' for the given id: '${envelopeId.value}'",
      StatusCodes.NotFound.intValue
    )
  }

  "delete" should "remove the retrievals with given id" in {
    authRetrievalMongoCache.upsert(retrievals).futureValue
    assert(authRetrievalMongoCache.find(envelopeId).futureValue.isDefined)

    val result = authRetrievalMongoCache.delete(envelopeId).futureValue
    result shouldBe a[Unit]
    authRetrievalMongoCache.find(envelopeId).futureValue shouldBe None
  }

  it should "ignore delete if retrievals not exists" in {
    val result = authRetrievalMongoCache.delete(envelopeId).futureValue
    result shouldBe a[Unit]
  }

}
