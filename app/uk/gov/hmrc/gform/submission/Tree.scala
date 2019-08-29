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

case class Tree[T](value: T, children: List[Tree[T]]) {
  def find(p: T => Boolean): Option[T] = toList.find(p)

  def map[U](f: T => U): Tree[U] = Tree(f(value), children.map(_.map(f)))

  def fold[U](seed: U)(f: (U, T) => U): U = children.foldLeft(f(seed, value)) { (acc, t) =>
    t.fold(acc)(f)
  }

  def toList(): List[T] =
    fold(List.empty[T]) { (acc, t) =>
      t :: acc
    }.reverse
}

object Tree {
  def apply[T](value: T, children: Tree[T]*): Tree[T] = Tree(value, children.toList)
}
