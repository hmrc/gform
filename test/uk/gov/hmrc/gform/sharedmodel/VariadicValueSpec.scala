/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }

class VariadicValueSpec extends Spec {
  "toSeq" should "turn One into a Seq of one element" in {
    One("foo").toSeq shouldBe Seq("foo")
  }

  it should "turn Many into a Seq of all elements" in {
    Many(Seq("foo", "bar")).toSeq shouldBe Seq("foo", "bar")
  }

  "exists" should "return false for One if the predicate does not match the value" in {
    One("foo").exists(_.equals("bar")) shouldBe false
  }

  it should "return true for One if the predicate does match the value" in {
    One("foo").exists(_.contains("o")) shouldBe true
  }

  it should "return false for Many if the predicate does not match any of the values" in {
    Many(Seq("foo")).exists(_.contains("bar")) shouldBe false
  }

  it should "return true for Many if the predicate does match any of the values" in {
    Many(Seq("foo", "bar")).exists(_.contains("o")) shouldBe true
  }

  it should "return false for an empty Many" in {
    Many(Seq.empty).exists(_.contains("o")) shouldBe false
  }

  "contains" should "return false for One if the value does not match" in {
    One("foo").contains("bar") shouldBe false
  }

  it should "return true for One if the predicate does match the value" in {
    One("foo").contains("foo") shouldBe true
  }

  it should "return false for Many if the predicate does not match any of the values" in {
    Many(Seq("foo")).contains("bar") shouldBe false
  }

  it should "return true for Many if the predicate does match any of the values" in {
    Many(Seq("foo", "bar")).contains("foo") shouldBe true
    Many(Seq("foo", "bar")).contains("bar") shouldBe true
  }

  it should "return false for an empty Many" in {
    Many(Seq.empty).contains("foo") shouldBe false
  }

  "map" should "return One for One with the value mapped" in {
    One("foo").map(_.toUpperCase()) shouldBe One("FOO")
  }

  it should "return Many for Many with all values mapped" in {
    Many(Seq("foo", "bar")).map(_.toUpperCase()) shouldBe Many(Seq("FOO", "BAR"))
  }

  "show" should "return the One value quoted" in {
    VariadicValue.show.show(One("foo")) shouldBe """"foo""""
  }

  it should "return the Many values quoted, surrounded by square brackets" in {
    VariadicValue.show.show(Many(Seq("foo", "bar"))) shouldBe """["foo", "bar"]"""
  }

  it should "return just square brackets for an empty Many" in {
    VariadicValue.show.show(Many(Seq.empty)) shouldBe """[]"""
  }
}
