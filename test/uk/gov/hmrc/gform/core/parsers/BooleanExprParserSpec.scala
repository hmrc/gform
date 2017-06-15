/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.core.parsers

import org.scalatest._

class BooleanExprParserSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  "IncludeIfParser" should "parse ${isPremisesSameAsBusinessAddress=0} | ${amountA=22}" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0} | ${amountA=22}")

    println(res)
  }

  "IncludeIfParser" should "parse ${isPremisesSameAsBusinessAddress=amountA} | ${amountA=22}" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=form.amountA} | ${amountA=22}")

    println(res)
  }

  it should "parse ${isPremisesSameAsBusinessAddress=0_0}" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0_0}")

    println(res)
  }

  it should "parse True" in {
    val res = BooleanExprParser.validate("True")

    println(res)
  }
}
