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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import org.scalatest.{ Matchers, WordSpecLike }
import play.api.libs.json._

class JsonUtilsSpec extends WordSpecLike with Matchers {

  trait Fixture {
    case class TestObject(id: String)
    val testObj = TestObject("1")
    val reads: Reads[TestObject] = (__ \ "id").read[String].map(TestObject)
    val writes: OWrites[TestObject] = new OWrites[TestObject] {
      override def writes(o: TestObject): JsObject = JsObject(Seq("id" -> JsString(o.id)))
    }
    val format: OFormat[TestObject] = OFormat(reads, writes)
    val dbIdOFormat: OFormat[TestObject] = JsonUtils.dbIdOFormat(format)
  }

  "dbIdOFormat" should {
    "move id attribute to _id on writes" in new Fixture {
      dbIdOFormat.writes(testObj) shouldEqual JsObject(Seq("_id" -> JsString(testObj.id)))
    }

    "move _id attribute to id on reads" in new Fixture {
      dbIdOFormat.reads(JsObject(Seq("_id" -> JsString(testObj.id)))).asOpt shouldBe Some(testObj)
    }
  }
}
