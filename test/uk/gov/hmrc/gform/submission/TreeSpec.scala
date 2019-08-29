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

package uk.gov.hmrc.gform.submission

import uk.gov.hmrc.gform.Spec

class TreeSpec extends Spec {
  "find" should "return None if no node matching the predicate is found" in {
    val tree: Tree[Int] = Tree(1, Tree(2, Tree(3)), Tree(4))
    tree.find(_ == 0) shouldBe None
  }

  it should "return the first matching node value if a node matching the predicate is found" in {
    val tree: Tree[String] = Tree("One", Tree("Two", Tree("Two again")), Tree("Four"))
    tree.find(_.contains("Tw")) shouldBe Some("Two")
    tree.find(_.contains("again")) shouldBe Some("Two again")
  }

  "map" should "return a tree with all node values mapped according to the given function" in {
    val tree: Tree[Int] = Tree(1, Tree(2, Tree(3)), Tree(4))
    tree.map(_ * 2) shouldBe Tree(2, Tree(4, Tree(6)), Tree(8))
  }

  "fold" should "traverse the tree depth first pre-order, applying the accumulator" in {
    val tree: Tree[Int] = Tree(1, Tree(2, Tree(3)), Tree(4))
    tree.fold(List.empty[Int]) { (acc, v) =>
      v :: acc
    } shouldBe List(4, 3, 2, 1)
  }

  "toList" should "produce a list of the values in fold order" in {
    val tree: Tree[Int] = Tree(1, Tree(2, Tree(3)), Tree(4))
    tree.toList shouldBe List(1, 2, 3, 4)
  }
}
