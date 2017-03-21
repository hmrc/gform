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
import uk.gov.hmrc.bforms.core.{Text => ComponentText}
import uk.gov.hmrc.bforms.exceptions.InvalidState
import uk.gov.hmrc.bforms.models._

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
      FieldValue("firstName", Some(ComponentText), "Your first name", None, None, None, None, None),
      FieldValue("lastName", Some(ComponentText), "Your last name", None, None, None, None, None)
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
