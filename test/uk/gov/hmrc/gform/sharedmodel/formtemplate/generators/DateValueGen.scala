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
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait DateValueGen {
  def exactDateValueGen: Gen[ExactDateValue] =
    for {
      year  <- Gen.posNum[Int]
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield ExactDateValue(year, month, day)

  def nextDateValueGen: Gen[NextDateValue] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield NextDateValue(month, day)

  def previousDateValueGen: Gen[PreviousDateValue] =
    for {
      month <- Gen.posNum[Int]
      day   <- Gen.posNum[Int]
    } yield PreviousDateValue(month, day)

  def dateValueGen: Gen[DateValue] = Gen.oneOf(
    Gen.const(TodayDateValue),
    exactDateValueGen,
    nextDateValueGen,
    previousDateValueGen
  )
}

object DateValueGen extends DateValueGen
