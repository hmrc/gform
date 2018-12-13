/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import cats.data.NonEmptyList
import org.scalacheck.Gen

object PrimitiveGen {
  def nonEmptyAlphaNumStrGen: Gen[String] = Gen.alphaNumStr.filter(_.nonEmpty)
  def booleanGen: Gen[Boolean] = Gen.oneOf(false, true)

  def urlGen: Gen[String] =
    for {
      d1          <- nonEmptyAlphaNumStrGen
      d2          <- nonEmptyAlphaNumStrGen
      d3          <- Gen.oneOf(".co.uk", "com")
      contextPath <- nonEmptyAlphaNumStrGen
    } yield s"https://$d1.$d2.$d3/$contextPath"

  def zeroOrMoreGen[T](gen: Gen[T]): Gen[List[T]] = Gen.oneOf(
    Gen.const(Nil),
    gen.map(List(_)),
    for {
      t1 <- gen
      t2 <- gen
    } yield List(t1, t2)
  )

  def oneOrMoreGen[T](gen: Gen[T]): Gen[NonEmptyList[T]] = Gen.oneOf(
    gen.map(NonEmptyList.of(_)),
    for {
      t1 <- gen
      t2 <- gen
    } yield NonEmptyList.of(t1, t2)
  )
}
