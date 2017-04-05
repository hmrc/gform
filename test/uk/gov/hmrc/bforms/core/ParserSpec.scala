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

import java.time.LocalDate

import org.scalatest._
import uk.gov.hmrc.bforms.core.utils.DateHelperFunctions
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.bforms.typeclasses.Now

class ParserSpec extends FlatSpec with Matchers with EitherValues with OptionValues {

  "Parser" should "parse ${firstName}" in {
    val res = Parser.validate("${firstName}")
    res.right.value should be(FormCtx("firstName"))
  }

  it should "parse ${eeitt.firstName}" in {
    val res = Parser.validate("${eeitt.firstName}")
    res.right.value should be(EeittCtx("firstName"))
  }

  it should "parse ${form.firstName}" in {
    val res = Parser.validate("${form.firstName}")
    res.right.value should be(FormCtx("firstName"))
  }

  it should "parse ${eeitt.firstName + form.secondName}" in {
    val res = Parser.validate("${eeitt.firstName + form.secondName}")
    res.right.value should be(Add(EeittCtx("firstName"), FormCtx("secondName")))
  }

  it should "parse ${eeitt.firstName * form.secondName}" in {
    val res = Parser.validate("${eeitt.firstName * form.secondName}")
    res.right.value should be(Multiply(EeittCtx("firstName"), FormCtx("secondName")))
  }

  it should "parse ${firstName * secondName}" in {
    val res = Parser.validate("${firstName * secondName}")
    res.right.value should be(Multiply(FormCtx("firstName"), FormCtx("secondName")))
  }

  it should "parse ${firstName * auth.secondName}" in {
    val res = Parser.validate("${firstName * auth.secondName}")
    res.right.value should be(Multiply(FormCtx("firstName"), AuthCtx("secondName")))
  }

  it should "parse constant" in {
    val res = Parser.validate("constant")
    res.right.value should be(Constant("constant"))
  }

  /**
   * Date cases
   */
  it should "parse Date" in {
    val res = Parser.validate("2015-01-15")
    res.right.value should be(DateExpr("15", "01", "2015"))
  }

  it should "parse Date as Constant" in {
    val res = Parser.validate("20150112")
    res.right.value should be(Constant("20150112"))
  }

  it should "throw exception on 1 digit month " in {
    val res = Parser.validate("2015-1-12")
    res.left.value should be(
      InvalidState(
        """Unable to parse expression 2015-1-12.
          |Errors:
          |2015-1-12:1: unexpected characters; expected '\s+'
          |2015-1-12    ^""".stripMargin
      )
    )
  }

  it should "throw exception on year digits" in {
    val res = Parser.validate("201568-01-12")
    res.left.value should be(
      InvalidState(
        """Unable to parse expression 201568-01-12.
          |Errors:
          |201568-01-12:1: unexpected characters; expected '\s+'
          |201568-01-12      ^""".stripMargin
      )
    )
  }

  it should "throw exception on Date format" in {
    val res = Parser.validate("65841-351")
    res.left.value should be(
      InvalidState(
        """Unable to parse expression 65841-351.
          |Errors:
          |65841-351:1: unexpected characters; expected '\s+'
          |65841-351     ^""".stripMargin
      )
    )
  }

  it should "parse next Date setting next year" in {
    val res = Parser.validate("next-01-15")

    implicit val now = Now(LocalDate.now())

    res.right.value should be(DateExpr("15", "01",
      (now.apply().getYear + 1).toString))
  }

  it should "parse next Date setting current year" in {
    val res = Parser.validate("next-04-15")
    implicit val now = Now(LocalDate.now())

    res.right.value should be(DateExpr("15", "04",
      now.apply().getYear.toString))
  }

  it should "parse last Date setting current year" in {
    val res = Parser.validate("last-01-15")

    implicit val localDateNow = Now(LocalDate.now())

    res.right.value should be(DateExpr("15", "01",
      localDateNow.apply().getYear.toString))
  }

  it should "parse last Date setting previous year" in {
    val res = Parser.validate("last-04-15")

    implicit val localDateNow = Now(LocalDate.now())

    res.right.value should be(DateExpr("15", "04",
      (localDateNow.apply().getYear - 1).toString))
  }

  it should "parse Date setting current Date" in {
    val res = Parser.validate("today")

    implicit val localDateNow = Now(LocalDate.now())

    res.right.value should be(DateExpr("%02d".format(localDateNow().getDayOfMonth), "%02d".format(localDateNow().getMonthValue()),
      localDateNow.apply().getYear.toString))
  }

  it should "fail parse unclosed parenthesis" in {
    val res = Parser.validate("${name")
    res.left.value should be(
      InvalidState(
        """|Unable to parse expression ${name.
           |Errors:
           |${name: unexpected end-of-file; expected '*' or '+' or '}'""".stripMargin
      )
    )
  }

  val plainFormTemplate = FormTemplate(FormTypeId("IPT100"), "Insurance Premium Tax Return", "version", "description", "characterSet", DmsSubmission("nino", "BT-NRU-Environmental", "FinanceOpsCorpT"), "submitSuccessUrl", "submitErrorUrl", List.empty[Section])

  val yourDetailsSection = Section(
    "Your details",
    List(
      FieldValue(FieldId("firstName"), Text, "Your first name", None, None, None, None, false, None),
      FieldValue(FieldId("lastName"), Text, "Your last name", None, None, None, None, false, None)
    )
  )

  val formTemplateWithOneSection = plainFormTemplate.copy(sections = List(yourDetailsSection))

  "Expr.validate" should "return Valid if expression include fieldName id present in the form template" in {
    val res = Expr.validate(List(FormCtx("firstName")), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Add fields present in the form template" in {
    val res = Expr.validate(List(Add(FormCtx("firstName"), FormCtx("lastName"))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Multiply fields present in the form template" in {
    val res = Expr.validate(List(Multiply(FormCtx("firstName"), FormCtx("lastName"))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Invalid if expression include fieldName id not present in the form template" in {
    val res = Expr.validate(List(FormCtx("firstNameTypo")), formTemplateWithOneSection)
    res should be(Invalid("Form field 'firstNameTypo' is not defined in form template."))
  }
}
