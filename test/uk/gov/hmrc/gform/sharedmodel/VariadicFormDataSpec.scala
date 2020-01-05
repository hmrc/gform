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

package uk.gov.hmrc.gform.sharedmodel

import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import VariadicValue.{ Many, One }
import VariadicFormData.{ manys, ones }

import scala.util.{ Failure, Success, Try }

class VariadicFormDataSpec extends Spec {
  private val aFormComponentId = FormComponentId("a")
  private val bFormComponentId = FormComponentId("b")
  private val cFormComponentId = FormComponentId("c")

  "get" should "return None if no value can be found" in {
    VariadicFormData.empty.get(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if a value can be found" in {
    val data = VariadicFormData(Map(aFormComponentId -> One("x"), bFormComponentId -> Many(Seq("y"))))

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("y")))
  }

  "one" should "return None if no value can be found" in {
    VariadicFormData.empty.one(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if the value can be found and is a One" in {
    VariadicFormData(Map(aFormComponentId -> One("x"))).one(aFormComponentId) shouldBe Some("x")
  }

  it should "throw an exception if the value can be found but is a Many" in {
    Try(VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).one(aFormComponentId)) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "oneOrElse" should "return the default value if not value can be found" in {
    VariadicFormData.empty.oneOrElse(aFormComponentId, "x") shouldBe "x"
  }

  it should "return the bound value if the value can be found and is a One" in {
    VariadicFormData(Map(aFormComponentId -> One("x"))).oneOrElse(aFormComponentId, "y") shouldBe "x"
  }

  it should "throw an exception if the value can be found but is a Many" in {
    Try(VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).oneOrElse(aFormComponentId, "y")) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "many" should "return None if no value can be found" in {
    VariadicFormData.empty.many(aFormComponentId) shouldBe None
  }

  it should "return Some(value) if the value can be found and is a Many" in {
    VariadicFormData(Map(aFormComponentId -> Many(Seq("x")))).many(aFormComponentId) shouldBe Some(Seq("x"))
  }

  it should "throw an exception if the value can be found but is a One" in {
    Try(VariadicFormData(Map(aFormComponentId -> One("x"))).many(aFormComponentId)) match {
      case Failure(_) =>
      case Success(_) => fail
    }
  }

  "++" should "combine all bindings in the two collections" in {
    val data = ones(aFormComponentId -> "x") ++
      manys(bFormComponentId         -> Seq("y"))

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("y")))
  }

  it should "overwrite bindings in the first collection with those in the second" in {
    val first = ones(aFormComponentId -> "x") ++
      manys(bFormComponentId          -> Seq("y"))

    val second = manys(bFormComponentId -> Seq("z"))

    val data = first ++ second

    data.get(aFormComponentId) shouldBe Some(One("x"))
    data.get(bFormComponentId) shouldBe Some(Many(Seq("z")))
  }

  "keySet" should "return all the keys" in {
    (ones(aFormComponentId   -> "x") ++
      manys(bFormComponentId -> Seq("y"))).keySet shouldBe Set(aFormComponentId, bFormComponentId)
  }

  "addValue" should "add the value" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addValue (bFormComponentId -> Many(Seq("y")))

    data.one(aFormComponentId) shouldBe Some("x")
    data.many(bFormComponentId) shouldBe Some(Seq("y"))
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addValue (aFormComponentId -> Many(Seq("y")))

    data.many(aFormComponentId) shouldBe Some(List("y"))
  }

  "addOne" should "add the value" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addOne (bFormComponentId -> "y")

    data.one(aFormComponentId) shouldBe Some("x")
    data.one(bFormComponentId) shouldBe Some("y")
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addOne (aFormComponentId -> "y")

    data.one(aFormComponentId) shouldBe Some("y")
  }

  "addMany" should "add the values" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addMany (bFormComponentId -> Seq("y"))

    data.one(aFormComponentId) shouldBe Some("x")
    data.many(bFormComponentId) shouldBe Some(Seq("y"))
  }

  it should "overwrite any existing binding with the same key" in {
    val initial = ones(aFormComponentId -> "x")

    val data = initial addMany (aFormComponentId -> Seq("y"))

    data.many(aFormComponentId) shouldBe Some(Seq("y"))
  }

  "--" should "remove all bindings with the given keys" in {
    val data = ones(aFormComponentId -> "x") ++
      manys(bFormComponentId         -> Seq("y"))

    data -- Set(aFormComponentId) shouldBe manys(bFormComponentId -> Seq("y"))
    data -- Set(bFormComponentId) shouldBe ones(aFormComponentId  -> "x")
  }

  it should "do nothing if the key doesn't have a binding" in {
    ones(aFormComponentId -> "x") -- Seq(bFormComponentId) shouldBe ones(aFormComponentId -> "x")
  }

  "collect" should "return the matching mapped values" in {
    val data =
      ones(aFormComponentId -> "One thing", bFormComponentId -> "Another One thing", cFormComponentId -> "Whatever")

    data.collect { case (k, One(v)) if v.contains("One") => k }.toSet shouldBe Set(aFormComponentId, bFormComponentId)
  }

  "contains" should "indicate if the FormComponentId has a binding" in {
    val data = ones(aFormComponentId -> "One thing", bFormComponentId -> "Another One thing")

    data.contains(aFormComponentId) shouldBe true
    data.contains(bFormComponentId) shouldBe true
    data.contains(cFormComponentId) shouldBe false
  }

  "mapValues" should "map the values" in {
    val data =
      ones(aFormComponentId    -> "Value") ++
        manys(bFormComponentId -> Seq("First", "Second"))

    data.mapValues {
      case (_, One(v))   => Many(Seq(v))
      case (_, Many(vs)) => One(vs.head)
    } should be
    manys(aFormComponentId  -> Seq("Value")) ++
      ones(bFormComponentId -> "First")
  }

  "buildFromMongoData" should "create values of the right VariadicValue type" in {
    VariadicFormData.buildFromMongoData(
      Set(aFormComponentId),
      Map(aFormComponentId -> "1, 2, ", bFormComponentId -> "3, 4, 5, ")
    ) shouldBe (VariadicFormData.manys(aFormComponentId -> Seq("1", "2")) ++ VariadicFormData.ones(
      bFormComponentId                                  -> "3, 4, 5, "))
  }
}
