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

package uk.gov.hmrc.gform.core.parsers

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations

class ValueParserSpec extends Spec {

  //TODO: use ExampleData

  "ValueParser" should "parse integer" in {
    val res = ValueParser.validate("${1}")
    res.right.value should be(TextExpression(Constant("1")))
  }
  it should "parse integer with decimal point" in {
    val res = ValueParser.validate("${1.}")
    res.right.value should be(TextExpression(Constant("1.")))
  }

  it should "parse multi digit integer" in {
    val res = ValueParser.validate("${12}")
    res.right.value should be(TextExpression(Constant("12")))
  }
  it should "parse an empty string" in {
    val res = ValueParser.validate("${''}")
    res.right.value should be(TextExpression(Constant("")))
  }
  it should "parse double digit integer" in {
    val res = ValueParser.validate("${1234}")
    res.right.value should be(TextExpression(Constant("1234")))
  }

  it should "parse multi digit integer with thousand separators" in {
    val res = ValueParser.validate("${1,234}")
    res.right.value should be(TextExpression(Constant("1,234")))
  }

  it should "parse double digit decimal fraction" in {
    val res = ValueParser.validate("${.12}")
    res.right.value should be(TextExpression(Constant(".12")))
  }

  it should "parse multi digit decimal fraction" in {
    val res = ValueParser.validate("${.123}")
    res.right.value should be(TextExpression(Constant(".123")))
  }

  it should "parse number plus decimal fraction" in {
    val res = ValueParser.validate("${1,234.1}")
    res.right.value should be(TextExpression(Constant("1,234.1")))
  }

  it should "parse number plus double digit decimal fraction" in {
    val res = ValueParser.validate("${1,234.12}")
    res.right.value should be(TextExpression(Constant("1,234.12")))
  }

  it should "parse number plus multi digit decimal fraction" in {
    val res = ValueParser.validate("${1,234.123}")
    res.right.value should be(TextExpression(Constant("1,234.123")))
  }

  it should "parse ${firstName}" in {
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

  it should "parse ${user.enrolledIdentifier}" in {
    val res = ValueParser.validate("${user.enrolledIdentifier}")
    res.right.value should be(TextExpression(UserCtx(EnrolledIdentifier)))
  }

  it should "fail to parse ${user.enrolledIdentifier" in {

    val res = ValueParser.validate("${user.enrolledIdentifier")
    res.left.value should be(UnexpectedState("""Unable to parse expression ${user.enrolledIdentifier.
                                               |Errors:
                                               |${user.enrolledIdentifier: unexpected end-of-file; expected '}'""".stripMargin))
  }

  it should "fail to parse ${user.enrolledIdentifiers}" in {

    val res = ValueParser.validate("${user.enrolledIdentifiers}")
    res.left.value should be(
      UnexpectedState("""Unable to parse expression ${user.enrolledIdentifiers}.
                        |Errors:
                        |${user.enrolledIdentifiers}:1: unexpected characters; expected '+' or '}' or '\s+' or '*' or '-'
                        |${user.enrolledIdentifiers}                         ^""".stripMargin))
  }

  it should "parse ${user.enrolments.<identifierName>.<referenceName>}" in {

    val validIdentifiersCombinations = Table(
      // format: off
      ("serviceName", "identifierName"),
      ("HMRC-AS-AGENT", "AgentReferenceNumber"),
      ("HMRC AS-AGENT", "Agent Reference Number"),
      ("test-{{}}}}", ")(*&^%$#@@@)")
      // format: on
    )

    val invalidIdentifiersCombinations = Table(
      // format: off
      ("serviceName", "identifierName"),
      ("HMRC.AA.AGENT", "AgentReferenceNumber"),  // serviceName    cannot include `.`
      ("HMRC-AS-AGENT", "Agent.ReferenceNumber"), // identifierName cannot include `.`
      ("HMRC-AS-AGENT", "Agent}ReferenceNumber")  // identifierName cannot include `}`
      // format: on
    )

    forAll(validIdentifiersCombinations) { (serviceName, identifierName) ⇒
      val res = ValueParser.validate(s"$${user.enrolments.$serviceName.$identifierName}")
      res.right.value should be(
        TextExpression(UserCtx(Enrolment(ServiceName(serviceName), IdentifierName(identifierName)))))
    }

    forAll(invalidIdentifiersCombinations) { (serviceName, identifierName) ⇒
      val res = ValueParser.validate("${user.enrolments." + serviceName + "." + identifierName + "}")
      res.left.value.error should include(
        s"Unable to parse expression $${user.enrolments.$serviceName.$identifierName}")
    }
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

  it should "parse ${a - b  * c}" in {
    val res = ValueParser.validate("${firstName - secondName * thirdname}")
    res.right.value should be(
      TextExpression(Subtraction(FormCtx("firstName"), Multiply(FormCtx("secondName"), FormCtx("thirdname")))))
  }

  it should "parse string constant" in {
    val res = ValueParser.validate("'constant'")
    res.right.value should be(TextExpression(Constant("constant")))
  }

  it should "parse string constant including space" in {
    val res = ValueParser.validate("'const ant'")
    res.right.value should be(TextExpression(Constant("const ant")))
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

  it should "parse lastDay" in {
    val res = ValueParser.validate("2015-01-lastDay")
    res.right.value should be(DateExpression(ExactDateValue(2015, 1, 31)))
  }

  it should "parse firstDay" in {
    val res = ValueParser.validate("2015-01-firstDay")
    res.right.value should be(DateExpression(ExactDateValue(2015, 1, 1)))
  }

  it should "throw exception on 1 digit month " in {
    val res = ValueParser.validate("2015-1-12")
    res.left.value should be(UnexpectedState("""Unable to parse expression 2015-1-12.
                                               |Errors:
                                               |2015-1-12:1: unexpected characters; expected '0[1-9]|1[012]' or '\s+'
                                               |2015-1-12     ^""".stripMargin))
  }

  it should "throw exception on year digits" in {
    val res = ValueParser.validate("201568-01-12")
    res.left.value should be(UnexpectedState("""Unable to parse expression 201568-01-12.
                                               |Errors:
                                               |201568-01-12:1: unexpected characters; expected '\s+' or ','
                                               |201568-01-12      ^""".stripMargin))
  }

  it should "throw exception on Date format" in {
    val res = ValueParser.validate("65841-351")
    res.left.value should be(UnexpectedState("""Unable to parse expression 65841-351.
                                               |Errors:
                                               |65841-351:1: unexpected characters; expected '\s+' or ','
                                               |65841-351     ^""".stripMargin))
  }

  it should "parse next Date setting next year" in {
    val res = ValueParser.validate("next-01-15")

    res.right.value should be(DateExpression(NextDateValue(1, 15)))
  }

  it should "parse next Date setting current year" in {
    val res = ValueParser.validate("next-04-15")

    res.right.value should be(DateExpression(NextDateValue(4, 15)))
  }

  it should "parse last Date setting current year" in {
    val res = ValueParser.validate("last-01-15")

    res.right.value should be(DateExpression(PreviousDateValue(1, 15)))
  }

  it should "parse last Date setting previous year" in {
    val res = ValueParser.validate("last-04-15")

    res.right.value should be(DateExpression(PreviousDateValue(4, 15)))
  }

  it should "parse Date setting current Date" in {
    val res = ValueParser.validate("today")

    res.right.value should be(DateExpression(TodayDateValue))
  }

  it should "parse submissionReference" in {
    val res = ValueParser.validate("${form.submissionReference}")

    res.right.value should be(TextExpression(SubmissionReference))
  }

  it should "fail parse unclosed parenthesis" in {
    val res = ValueParser.validate("${name")
    res.left.value should be(UnexpectedState("""|Unable to parse expression ${name.
                                                |Errors:
                                                |${name: unexpected end-of-file; expected '}'""".stripMargin))
  }

  val plainFormTemplate = FormTemplate.withDeprecatedDmsSubmission(
    FormTemplateId("IPT100"),
    "Insurance Premium Tax Return",
    "description",
    Some(ResearchBanner),
    Some(Default),
    None,
    None,
    Destinations
      .DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "BT-NRU-Environmental", "FinanceOpsCorpT"),
    HmrcAgentWithEnrolmentModule(
      RequireMTDAgentEnrolment,
      EnrolmentAuth(ServiceId("TEST"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("TEST"))))),
    "test-email-template-id",
    Some(
      NonEmptyList.of(
        EmailParameter("fullName", TextExpression(FormCtx("directorFullName"))),
        EmailParameter("email", TextExpression(FormCtx("directorEmail")))
      )),
    "submitSuccessUrl",
    "submitErrorUrl",
    List.empty[Section],
    acknowledgementSection = AcknowledgementSection("", None, None, Nil),
    declarationSection = DeclarationSection("Declaration", None, None, Nil)
  )

  val yourDetailsSection = Section(
    "Your details",
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    List(
      FormComponent(
        FormComponentId("firstName"),
        Text(AnyText, Value),
        "Your first name",
        None,
        None,
        validIf = None,
        mandatory = false,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      ),
      FormComponent(
        FormComponentId("lastName"),
        Text(AnyText, Value),
        "Your last name",
        None,
        None,
        validIf = None,
        mandatory = false,
        editable = true,
        submissible = true,
        derived = false,
        errorMessage = None
      )
    ),
    None,
    None
  )

  val formTemplateWithOneSection = plainFormTemplate.copy(sections = List(yourDetailsSection))

  "Expr.validate" should "return Valid if expression include fieldName id present in the form template" in {

    val res = FormTemplateValidator
      .validate(List(Text(AnyText, FormCtx("firstName"))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Add fields present in the form template" in {
    val res =
      FormTemplateValidator
        .validate(List(Text(AnyText, Add(FormCtx("firstName"), FormCtx("lastName")))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Multiply fields present in the form template" in {
    val res =
      FormTemplateValidator
        .validate(List(Text(AnyText, Multiply(FormCtx("firstName"), FormCtx("lastName")))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Invalid if expression include fieldName id not present in the form template" in {
    val res = FormTemplateValidator
      .validate(List(Text(AnyText, FormCtx("firstNameTypo"))), formTemplateWithOneSection)
    res should be(Invalid("Form field 'firstNameTypo' is not defined in form template."))
  }
  it should "return invalid and not be parsed as empty string" in {
    val res = FormTemplateValidator
      .validate(List(Text(AnyText, FormCtx("'' * ''"))), formTemplateWithOneSection)
    res should be(Invalid("Form field ''' * ''' is not defined in form template."))
  }

}
