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
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprParserSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  "BooleanExprParser" should "parse or-expressions" in {
    val res = BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0} || ${amountA=22}")

    res shouldBe Right(Or(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0")), Equals(FormCtx("amountA"), Constant("22"))))

  }

  "BooleanExprParser" should "parse or-expressions inside form context" in {
    val res = BooleanExprParser.validate("${hasOrgsAddressChanged=1 || hasOrgsAddressChanged=0}")

    res shouldBe Right(Or(
      Equals(FormCtx("hasOrgsAddressChanged"), Constant("1")),
      Equals(FormCtx("hasOrgsAddressChanged"), Constant("0"))))

  }

  it should "fail to parse anything but an equals operator" in {
    val res = BooleanExprParser.validate("${abc>form.amountA}")

    res should be('left)

    res.left.value match {
      case UnexpectedState(msg) => msg.contains("expected '=' or '\\s+'") shouldBe true
      case _ => fail("expected an UnexpectedState")
    }
  }

  it should "fail to parse anything but a constant on the right size" in {
    val res = BooleanExprParser.validate("${abc=form.amountA}")

    res should be('left)

    def pointToFirstUnexpectedCharacter(parserMsg: String) = {
      val spacesBeforeCaret = "[ ]+(?=\\^)".r.unanchored.findAllIn(parserMsg).toList.last
      spacesBeforeCaret.size shouldBe ("${abc=form".size)
    }

    res.left.value match {
      case UnexpectedState(msg) => pointToFirstUnexpectedCharacter(msg)
      case _ => fail("expected an UnexpectedState")
    }
  }

  it should "parse ${form.isPremisesSameAsBusinessAddress=amountA}" in {
    val res = BooleanExprParser.validate("${eeitt.businessUserx = XYZ}")

    res should be('left)

    res.left.value should be(
      UnexpectedState(
        """
Unable to parse expression ${eeitt.businessUserx = XYZ}.
Errors:
${eeitt.businessUserx = XYZ}:1: unexpected characters; expected '=' or '\s+'
${eeitt.businessUserx = XYZ}                    ^
    """.trim))
  }

  it should "parse ${isPremisesSameAsBusinessAddress=0_0}" in {
    BooleanExprParser.validate("${isPremisesSameAsBusinessAddress=0_0}") shouldBe Right(Equals(FormCtx("isPremisesSameAsBusinessAddress"), Constant("0_0")))
  }

  it should "parse True" in {
    BooleanExprParser.validate("True") shouldBe Right(IsTrue)
  }
}
