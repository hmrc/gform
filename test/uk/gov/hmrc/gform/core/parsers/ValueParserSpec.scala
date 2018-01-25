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

package uk.gov.hmrc.gform.core.parsers

import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ValueParserSpec extends Spec {

  //TODO: use ExampleData

  "ValueParser" should "parse ${firstName}" in {
    val res = ValueParser.validate("${firstName}")
    res.right.value should be(TextExpression(FormCtx("firstName")))
  }

  it should "parse ${age - 1}" in {
    val res = ValueParser.validate("${age - 1}")
    res.right.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${age -1}" in {
    val res = ValueParser.validate("${age -1}")
    res.right.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${age-1}" in {
    val res = ValueParser.validate("${age-1}")
    res.right.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${eeitt.businessUser}" in {
    val res = ValueParser.validate("${eeitt.businessUser}")
    res.right.value should be(TextExpression(EeittCtx(BusinessUser)))
  }

  it should "parse ${eeitt.agent}" in {
    val res = ValueParser.validate("${eeitt.agent}")
    res.right.value should be(TextExpression(EeittCtx(Agent)))
  }

  it should "parse ${eeitt.userId}" in {
    val res = ValueParser.validate("${eeitt.userId}")
    res.right.value should be(TextExpression(EeittCtx(UserId)))
  }

  it should "parse ${form.firstName}" in {
    val res = ValueParser.validate("${form.firstName}")
    res.right.value should be(TextExpression(FormCtx("firstName")))
  }

  it should "parse ${someId.sum}" in {
    val res = ValueParser.validate("${age.sum}")

    res.right.value should be(TextExpression(Sum(FormCtx("age"))))
  }

  it should "parse ${eeitt.firstName + form.secondName}" in {
    val res = ValueParser.validate("${eeitt.businessUser + form.secondName}")
    res.right.value should be(TextExpression(Add(EeittCtx(BusinessUser), FormCtx("secondName"))))
  }

  it should "parse ${eeitt.firstName * form.secondName}" in {
    val res = ValueParser.validate("${eeitt.businessUser * form.secondName}")
    res.right.value should be(TextExpression(Multiply(EeittCtx(BusinessUser), FormCtx("secondName"))))
  }

  it should "parse ${firstName * secondName}" in {
    val res = ValueParser.validate("${firstName * secondName}")
    res.right.value should be(TextExpression(Multiply(FormCtx("firstName"), FormCtx("secondName"))))
  }

  it should "parse ${firstName * auth.secondName}" in {
    val res = ValueParser.validate("${firstName * auth.sautr}")
    res.right.value should be(TextExpression(Multiply(FormCtx("firstName"), AuthCtx(SaUtr))))
  }

  it should "parse constant" in {
    val res = ValueParser.validate("'constant'")
    res.right.value should be(TextExpression(Constant("constant")))
  }

  it should "parse number as a choice selections" in {
    val res = ValueParser.validate("1")
    res.right.value should be(ChoiceExpression(List(1)))
  }

  it should "parse numbers separated by comma as a choice selections" in {
    val res = ValueParser.validate("1,2,3,4")
    res.right.value should be(ChoiceExpression(List(1, 2, 3, 4)))
  }

  /**
   * Date cases
   */
  it should "parse Date" in {
    val res = ValueParser.validate("2015-01-15")
    res.right.value should be(DateExpression(ExactDateValue(2015, 1, 15)))
  }

  it should "throw exception on 1 digit month " in {
    val res = ValueParser.validate("2015-1-12")
    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression 2015-1-12.
          |Errors:
          |2015-1-12:1: unexpected characters; expected '0[1-9]|1[012]' or '\s+'
          |2015-1-12     ^""".stripMargin))
  }

  it should "throw exception on year digits" in {
    val res = ValueParser.validate("201568-01-12")
    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression 201568-01-12.
          |Errors:
          |201568-01-12:1: unexpected characters; expected '\s+' or ','
          |201568-01-12      ^""".stripMargin))
  }

  it should "throw exception on Date format" in {
    val res = ValueParser.validate("65841-351")
    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression 65841-351.
          |Errors:
          |65841-351:1: unexpected characters; expected '\s+' or ','
          |65841-351     ^""".stripMargin))
  }

  it should "parse next Date setting next year" in {
    val res = ValueParser.validate("next-01-15")

    res.right.value should be(
      DateExpression(
        NextDateValue(1, 15)))
  }

  it should "parse next Date setting current year" in {
    val res = ValueParser.validate("next-04-15")

    res.right.value should be(
      DateExpression(
        NextDateValue(4, 15)))
  }

  it should "parse last Date setting current year" in {
    val res = ValueParser.validate("last-01-15")

    res.right.value should be(
      DateExpression(
        PreviousDateValue(1, 15)))
  }

  it should "parse last Date setting previous year" in {
    val res = ValueParser.validate("last-04-15")

    res.right.value should be(
      DateExpression(
        PreviousDateValue(4, 15)))
  }

  it should "parse Date setting current Date" in {
    val res = ValueParser.validate("today")

    res.right.value should be(
      DateExpression(TodayDateValue))
  }

  it should "fail parse unclosed parenthesis" in {
    val res = ValueParser.validate("${name")
    res.left.value should be(
      UnexpectedState(
        """|Unable to parse expression ${name.
           |Errors:
           |${name: unexpected end-of-file; expected '}'""".stripMargin))
  }

  val plainFormTemplate = FormTemplate(
    FormTemplateId("IPT100"),
    "Insurance Premium Tax Return",
    "description",
    None,
    Some(Default),
    None,
    DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "BT-NRU-Environmental", "FinanceOpsCorpT"),
    HMRCAuthConfigWithRegimeId(AuthConfigModule("TEST"), ServiceId("TEST"), RegimeId("TEST")),
    "submitSuccessUrl",
    "submitErrorUrl",
    List.empty[Section],
    acknowledgementSection = AcknowledgementSection("", None, None, Nil),
    declarationSection = DeclarationSection("Declaration", None, None, Nil))

  val yourDetailsSection = Section(
    "Your details",
    None, None, None,
    None, None, None,
    List(
      FormComponent(FormComponentId("firstName"), Text(AnyText, Constant("")), "Your first name", None, None, mandatory = false, editable = true, submissible = true, derived = false, errorMessage = None),
      FormComponent(FormComponentId("lastName"), Text(AnyText, Constant("")), "Your last name", None, None, mandatory = false, editable = true, submissible = true, derived = false, errorMessage = None)))

  val formTemplateWithOneSection = plainFormTemplate.copy(sections = List(yourDetailsSection))

  "Expr.validate" should "return Valid if expression include fieldName id present in the form template" in {

    val res = FormTemplateValidator.validate(List(Text(AnyText, FormCtx("firstName"))), formTemplateWithOneSection)
    println(Json.prettyPrint(Json.toJson(plainFormTemplate)))
    res should be(Valid)
  }

  it should "return Valid if expression Add fields present in the form template" in {
    val res = FormTemplateValidator.validate(List(Text(AnyText, Add(FormCtx("firstName"), FormCtx("lastName")))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Multiply fields present in the form template" in {
    val res = FormTemplateValidator.validate(List(Text(AnyText, Multiply(FormCtx("firstName"), FormCtx("lastName")))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Invalid if expression include fieldName id not present in the form template" in {
    val res = FormTemplateValidator.validate(List(Text(AnyText, FormCtx("firstNameTypo"))), formTemplateWithOneSection)
    res should be(Invalid("Form field 'firstNameTypo' is not defined in form template."))
  }
}
