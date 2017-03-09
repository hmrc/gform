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

package uk.gov.hmrc.bforms.core

import org.scalatest._
import parseback.ParseError
import parseback.util.Catenable

class ParserSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  "Parser" should "parse ${firstName}" in {
    val res = Parser("${firstName}")
    result(res) should be(Form("firstName"))
  }

  it should "parse ${eeitt.firstName}" in {
    val res = Parser("${eeitt.firstName}")
    result(res) should be(Eeitt("firstName"))
  }

  it should "parse ${form.firstName}" in {
    val res = Parser("${form.firstName}")
    result(res) should be(Form("firstName"))
  }

  it should "parse ${eeitt.firstName + form.secondName}" in {
    val res = Parser("${eeitt.firstName + form.secondName}")
    result(res) should be(Add(Eeitt("firstName"), Form("secondName")))
  }

  it should "parse ${eeitt.firstName * form.secondName}" in {
    val res = Parser("${eeitt.firstName * form.secondName}")
    result(res) should be(Multiply(Eeitt("firstName"), Form("secondName")))
  }

  it should "parse ${firstName * secondName}" in {
    val res = Parser("${firstName * secondName}")
    result(res) should be(Multiply(Form("firstName"), Form("secondName")))
  }

  it should "parse ${firstName * auth.secondName}" in {
    val res = Parser("${firstName * auth.secondName}")
    result(res) should be(Multiply(Form("firstName"), Auth("secondName")))
  }

  it should "parse constant" in {
    val res = Parser("constant")
    result(res) should be(Constant("constant"))
  }

  private def result(res: Either[List[ParseError], Catenable[Expr]]): Expr = {
    res.right.value.uncons.map(_._1).value
  }
}
