/*
 * Copyright 2023 HM Revenue & Customs
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

import munit.FunSuite

import cats.data.NonEmptyList
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.Helpers._
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.{ NewForm, NewFormForTemplate, NewSession, PageLink, PrintAcknowledgementHtml, PrintSectionNotificationPdf, PrintSectionPdf, SignOut }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.Dms
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, DataRetrieve, DataRetrieveId }

import scala.language.implicitConversions

class ValueParserSpec extends FunSuite with Matchers with OptionValues with TableDrivenPropertyChecks {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  test("ValueParser should desugar a bare data retrieve hasFailed boolean and its negation") {
    val condition = Equals(
      DataRetrieveCtx(DataRetrieveId("agencyInfo"), DataRetrieve.hasFailedAttribute),
      Constant("true")
    )
    val cases = Table(
      ("expression", "expected"),
      ("${dataRetrieve.agencyInfo.hasFailed}", condition),
      ("${!dataRetrieve.agencyInfo.hasFailed}", Not(condition)),
      ("${!(dataRetrieve.agencyInfo.hasFailed)}", Not(condition)),
      ("${dataRetrieve.agencyInfo.hasFailed && true}", And(condition, IsTrue)),
      ("${dataRetrieve.agencyInfo.hasFailed || true}", Or(condition, IsTrue)),
      ("${dataRetrieve.agencyInfo.hasFailed = 'true'}", condition),
      ("${dataRetrieve.agencyInfo.hasFailed = 'false'}", condition.copy(right = Constant("false")))
    )
    forAll(cases) { (expression, expected) =>
      ValueParser.validateWithParser(expression, ValueParser.booleanExpr) shouldBe Right(expected)
    }
    ValueParser.validateWithParser("${dataRetrieve.agencyInfo.isBlocked}", ValueParser.booleanExpr).isLeft shouldBe true
    ValueParser
      .validateWithParser("${dataRetrieve.agencyInfo.hasFailedOther}", ValueParser.booleanExpr)
      .isLeft shouldBe true
    ValueParser.validateWithParser("${dataRetrieve.agencyInfo.hasFailed}", ValueParser.expr) shouldBe Right(
      DataRetrieveCtx(DataRetrieveId("agencyInfo"), DataRetrieve.hasFailedAttribute)
    )
  }

  test("ValueParser should parse integer") {
    val res = ValueParser.validate("${1}")
    res.toOption.value should be(TextExpression(Constant("1")))
  }

  test("ValueParser should parse constant") {
    val res = ValueParser.validate("${' '}")
    res.toOption.value should be(TextExpression(Constant(" ")))
  }

  test("ValueParser should parse integer with decimal point") {
    val res = ValueParser.validate("${1.}")
    res.toOption.value should be(TextExpression(Constant("1.")))
  }

  test("ValueParser should parse multi digit integer") {
    val res = ValueParser.validate("${12}")
    res.toOption.value should be(TextExpression(Constant("12")))
  }
  test("ValueParser should parse an empty string") {
    val res = ValueParser.validate("${''}")
    res.toOption.value should be(TextExpression(Constant("")))
  }
  test("ValueParser should parse an anything except singe quote") {
    val res = ValueParser.validate("${' --+===> '}")
    res.toOption.value should be(TextExpression(Constant(" --+===> ")))
  }
  test("ValueParser should parse double digit integer") {
    val res = ValueParser.validate("${1234}")
    res.toOption.value should be(TextExpression(Constant("1234")))
  }

  test("ValueParser should parse multi digit integer with thousand separators") {
    val res = ValueParser.validate("${1,234}")
    res.toOption.value should be(TextExpression(Constant("1,234")))
  }

  test("ValueParser should parse double digit decimal fraction") {
    val res = ValueParser.validate("${.12}")
    res.toOption.value should be(TextExpression(Constant(".12")))
  }

  test("ValueParser should parse multi digit decimal fraction") {
    val res = ValueParser.validate("${.123}")
    res.toOption.value should be(TextExpression(Constant(".123")))
  }

  test("ValueParser should parse number plus decimal fraction") {
    val res = ValueParser.validate("${1,234.1}")
    res.toOption.value should be(TextExpression(Constant("1,234.1")))
  }

  test("ValueParser should parse number plus double digit decimal fraction") {
    val res = ValueParser.validate("${1,234.12}")
    res.toOption.value should be(TextExpression(Constant("1,234.12")))
  }

  test("ValueParser should parse number plus multi digit decimal fraction") {
    val res = ValueParser.validate("${1,234.123}")
    res.toOption.value should be(TextExpression(Constant("1,234.123")))
  }

  test("ValueParser should parse ${firstName}") {
    val res = ValueParser.validate("${firstName}")
    res.toOption.value should be(TextExpression(FormCtx("firstName")))
  }

  test("ValueParser should parse ${age - 1}") {
    val res = ValueParser.validate("${age - 1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  test("ValueParser should parse ${age -1}") {
    val res = ValueParser.validate("${age -1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  test("ValueParser should parse ${age-1}") {
    val res = ValueParser.validate("${age-1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  test("ValueParser should parse ${form.firstName}") {
    val res = ValueParser.validate("${form.firstName}")
    res.toOption.value should be(TextExpression(FormCtx("firstName")))
  }

  test("ValueParser should parse ${user.enrolledIdentifier}") {
    val res = ValueParser.validate("${user.enrolledIdentifier}")
    res.toOption.value should be(TextExpression(UserCtx(UserField.EnrolledIdentifier)))
  }

  test("ValueParser should parse anything except singe quote") {
    val res = ValueParser.validate("${' --+===>., '}")
    res.toOption.value should be(TextExpression(Constant(" --+===>., ")))
  }

  test("ValueParser should fail to parse ${user.enrolledIdentifier") {

    val res = ValueParser.validate("${user.enrolledIdentifier")
    res.left.value should be(
      UnexpectedState("""Unable to parse expression ${user.enrolledIdentifier.
                        |Errors:
                        |'}' expected but end of source found""".stripMargin)
    )
  }

  test("ValueParser should fail to parse ${user.enrolledIdentifiers}") {

    val res = ValueParser.validate("${user.enrolledIdentifiers}")
    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression ${user.enrolledIdentifiers}.
          |Errors:
          |'}' expected but 's' found""".stripMargin
      )
    )
  }

  test("ValueParser should parse ${user.credentialRole}") {
    val res = ValueParser.validate("${user.credentialRole}")
    res.toOption.value should be(TextExpression(UserCtx(UserField.CredentialRole)))
  }

  test("ValueParser should parse ${user.enrolments.<identifierName>.<referenceName>}") {

    val validIdentifiersCombinations = Table(
      // format: off
      ("serviceName", "identifierName"),
      ("HMRC-AS-AGENT", "AgentReferenceNumber"),
      ("test-)(*&^%$#@@@)", ")(*&^%$#@@@)")
      // format: on
    )

    val invalidIdentifiersCombinations = Table(
      // format: off
      ("serviceName", "identifierName"),
      ("HMRC.AA.AGENT", "AgentReferenceNumber"),  // serviceName    cannot include `.`
      ("HMRC AS-AGENT", "AgentReferenceNumber"),  // serviceName    cannot include ` `
      ("HMRC-AS-AGENT", "Agent.ReferenceNumber"), // identifierName cannot include `.`
      ("HMRC-AS-AGENT", "Agent}ReferenceNumber"), // identifierName cannot include `}`
      ("HMRC-AS-AGENT", "Agent ReferenceNumber"), // identifierName cannot include ` `
      ("HMRC-AS-AGENT", "Agent=ReferenceNumber")  // identifierName cannot include `=`
      // format: on
    )

    forAll(validIdentifiersCombinations) { (serviceName, identifierName) =>
      val res = ValueParser.validate(s"$${user.enrolments.$serviceName.$identifierName}")
      res.toOption.value should be(
        TextExpression(UserCtx(UserField.Enrolment(ServiceName(serviceName), IdentifierName(identifierName), None)))
      )
    }

    forAll(invalidIdentifiersCombinations) { (serviceName, identifierName) =>
      val res = ValueParser.validate("${user.enrolments." + serviceName + "." + identifierName + "}")
      res.left.value.error should include(
        s"Unable to parse expression $${user.enrolments.$serviceName.$identifierName}"
      )
    }
  }

  test("ValueParser should parse ${someId.sum}") {
    val res = ValueParser.validate("${age.sum}")

    res.toOption.value should be(TextExpression(Sum(FormCtx("age"))))
  }

  test("ValueParser should parse ${firstName * secondName}") {
    val res = ValueParser.validate("${firstName * secondName}")
    res.toOption.value should be(TextExpression(Multiply(FormCtx("firstName"), FormCtx("secondName"))))
  }

  test("ValueParser should parse ${firstName * auth.secondName}") {
    val res = ValueParser.validate("${firstName * auth.sautr}")
    res.toOption.value should be(TextExpression(Multiply(FormCtx("firstName"), AuthCtx(AuthInfo.SaUtr))))
  }

  test("ValueParser should parse ${auth.email}") {
    val res = ValueParser.validate("${auth.email}")
    res.toOption.value should be(TextExpression(AuthCtx(AuthInfo.EmailId)))
  }

  test("ValueParser should parse ${a - b  * c}") {
    val res = ValueParser.validate("${firstName - secondName * thirdname}")
    res.toOption.value should be(
      TextExpression(Subtraction(FormCtx("firstName"), Multiply(FormCtx("secondName"), FormCtx("thirdname"))))
    )
  }

  test("ValueParser should parse ${a * b  + c}") {
    val res = ValueParser.validate("${firstName * secondName + thirdname}")
    res.toOption.value should be(
      TextExpression(Add(Multiply(FormCtx("firstName"), FormCtx("secondName")), FormCtx("thirdname")))
    )
  }

  test("ValueParser should parse ${a / b}") {
    val res = ValueParser.validate("${firstName / secondName}")
    res.toOption.value should be(
      TextExpression(Divide(FormCtx("firstName"), FormCtx("secondName")))
    )
  }

  test("ValueParser should parse ${a * b  / c}") {
    val res = ValueParser.validate("${firstName * secondName / thirdname}")
    res.toOption.value should be(
      TextExpression(Divide(Multiply(FormCtx("firstName"), FormCtx("secondName")), FormCtx("thirdname")))
    )
  }

  test("ValueParser should parse string constant") {
    val res = ValueParser.validate("'constant'")
    res.toOption.value should be(TextExpression(Constant("constant")))
  }

  test("ValueParser should parse string constant including space") {
    val res = ValueParser.validate("'const ant'")
    res.toOption.value should be(TextExpression(Constant("const ant")))
  }

  test("ValueParser should parse number as a choice selections") {
    val res = ValueParser.validate("1")
    res.toOption.value should be(ChoiceExpression(List(1)))
  }

  test("ValueParser should parse numbers separated by comma as a choice selections") {
    val res = ValueParser.validate("1,2,3,4")
    res.toOption.value should be(ChoiceExpression(List(1, 2, 3, 4)))
  }

  /** Date cases
    */
  test("ValueParser should parse Date") {
    val res = ValueParser.validate("2015-01-15")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 15)))
  }

  test("ValueParser should parse lastDay") {
    val res = ValueParser.validate("2015-01-lastDay")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 31)))
  }

  test("ValueParser should parse firstDay") {
    val res = ValueParser.validate("2015-01-firstDay")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 1)))
  }

  test("ValueParser should throw exception on 1 digit month ") {
    val res = ValueParser.validate("2015-1-12")
    res.left.value should be(
      UnexpectedState("""Unable to parse expression 2015-1-12.
                        |Errors:
                        |end of input expected""".stripMargin)
    )
  }

  test("ValueParser should throw exception on year digits") {
    val res = ValueParser.validate("201568-01-12")
    res.left.value should be(UnexpectedState("""Unable to parse expression 201568-01-12.
                                               |Errors:
                                               |end of input expected""".stripMargin))
  }

  test("ValueParser should throw exception on Date format") {
    val res = ValueParser.validate("65841-351")
    res.left.value should be(UnexpectedState("""Unable to parse expression 65841-351.
                                               |Errors:
                                               |end of input expected""".stripMargin))
  }

  test("ValueParser should parse next Date setting next year") {
    val res = ValueParser.validate("next-01-15")

    res.toOption.value should be(DateExpression(NextDateValue(1, 15)))
  }

  test("ValueParser should parse next Date setting current year") {
    val res = ValueParser.validate("next-04-15")

    res.toOption.value should be(DateExpression(NextDateValue(4, 15)))
  }

  test("ValueParser should parse last Date setting current year") {
    val res = ValueParser.validate("last-01-15")

    res.toOption.value should be(DateExpression(PreviousDateValue(1, 15)))
  }

  test("ValueParser should parse last Date setting previous year") {
    val res = ValueParser.validate("last-04-15")

    res.toOption.value should be(DateExpression(PreviousDateValue(4, 15)))
  }

  test("ValueParser should parse Date setting current Date") {
    val res = ValueParser.validate("today")

    res.toOption.value should be(DateExpression(TodayDateValue))
  }

  test("ValueParser should parse submissionReference") {
    val res = ValueParser.validate("${form.submissionReference}")

    res.toOption.value should be(TextExpression(FormTemplateCtx(FormTemplateProp.SubmissionReference)))
  }

  test("ValueParser should parse fileSizeLimit") {
    val res = ValueParser.validate("${form.fileSizeLimit}")

    res.toOption.value should be(TextExpression(FormTemplateCtx(FormTemplateProp.FileSizeLimit)))
  }

  test("ValueParser should parse orElse expression") {
    implicit def liftToFormCtx(s: String): FormCtx = FormCtx(s)
    val table = Table(
      ("expression", "catenable"),
      // format: off
      ("a + b + c + d",              Add(Add(Add("a", "b"), "c"), "d")),
      ("(a + b) + c + d",            Add(Add(Add("a", "b"), "c"), "d")),
      ("(a + b + c) + d",            Add(Add(Add("a", "b"), "c"), "d")),
      ("((a + b) + c) + d",          Add(Add(Add("a", "b"), "c"), "d")),
      ("a + (b + c + d)",            Add("a", Add(Add("b", "c"), "d"))),
      ("a + (b + (c + d))",          Add("a", Add("b", Add("c", "d")))),
      ("(a + b) + (c + d)",          Add(Add("a", "b"), Add("c", "d"))),
      ("a + b - c + d",              Add(Add("a", Subtraction("b", "c")), "d")),
      ("a + (b - c) + d",            Add(Add("a", Subtraction("b", "c")), "d")),
      ("(a + b) - (c + d)",          Subtraction(Add("a", "b"), Add("c", "d"))),
      ("a + b * c + d",              Add(Add("a", Multiply("b", "c")), "d")),
      ("a + (b * c) + d",            Add(Add("a", Multiply("b", "c")), "d")),
      ("(a + b) * (c + d)",          Multiply(Add("a", "b"), Add("c", "d"))),
      ("a + b orElse c + d",           Add(Add("a", Else("b", "c")), "d")),
      ("a + (b orElse c) + d",         Add(Add("a", Else("b", "c")), "d")),
      ("(a + b) orElse (c + d)",       Else(Add("a", "b"), Add("c", "d"))),
      ("a - b + c - d",              Add(Subtraction("a", "b"), Subtraction("c", "d"))),
      ("a - b - c - d",              Subtraction(Subtraction(Subtraction("a", "b"), "c"), "d")),
      ("a - (b - (c - d))",          Subtraction("a", Subtraction("b", Subtraction("c", "d")))),
      ("a - b * c - d",              Subtraction(Subtraction("a", Multiply("b", "c")), "d")),
      ("a - b orElse c - d",           Subtraction(Subtraction("a", Else("b", "c")), "d")),
      ("a * b + c * d",              Add(Multiply("a", "b"), Multiply("c", "d"))),
      ("a * b - c * d",              Subtraction(Multiply("a", "b"), Multiply("c", "d"))),
      ("a * b * c * d",              Multiply(Multiply(Multiply("a", "b"), "c"), "d")),
      ("a * (b * (c * d))",          Multiply("a", Multiply("b", Multiply("c", "d")))),
      ("a * b orElse c * d",           Multiply(Multiply("a", Else("b", "c")), "d")),
      ("a orElse b + c orElse d",        Add(Else("a", "b"), Else("c", "d"))),
      ("a orElse b - c orElse d",        Subtraction(Else("a", "b"), Else("c", "d"))),
      ("a orElse b * c orElse d",        Multiply(Else("a", "b"), Else("c", "d"))),
      ("a orElse b orElse c orElse d",     Else("a", Else("b", Else("c", "d")))),
      ("a orElse (b orElse (c orElse d))", Else("a", Else("b", Else("c", "d")))),
      ("a / b / c / d",              Divide(Divide(Divide("a", "b"), "c"), "d")),
      ("a + b / c * d",              Add("a", Divide("b", Multiply("c", "d"))))
      // format: on
    )

    forAll(table) { (expression, expected) =>
      val res = ValueParser.validate("${" + expression + "}")

      res.toOption.value shouldBe TextExpression(expected)
    }
  }

  test("ValueParser should else expressions should be rewriten to nesting on right hand side") {
    implicit def liftToFormCtx(s: String): FormCtx = FormCtx(s)
    val table = Table(
      ("expression", "catenable"),
      // format: off
      (Add(Add(Add("a", "b"), "c"), "d"),               Add(Add(Add("a", "b"), "c"), "d")),
      (Else(Add("a", "b"), Add("c", "d")),              Else(Add("a", "b"), Add("c", "d"))),
      (Else(Else(Else(Else("a", "b"), "c"), "d"), "e"), Else("a", Else("b", Else("c", Else("d", "e"))))),
      (Else("a", Else("b", Else("c", "d"))),            Else("a", Else("b", Else("c", "d")))),
      (Add(Else("a", "b"), Else("c", "d")),             Add(Else("a", "b"), Else("c", "d"))),
      (Add(Else(Else("a", "b"), "c"), "d"),             Add(Else("a", Else("b", "c")), "d")),
      (Subtraction(Else(Else("a", "b"), "c"), "d"),     Subtraction(Else("a", Else("b", "c")), "d")),
      (Multiply(Else(Else("a", "b"), "c"), "d"),        Multiply(Else("a", Else("b", "c")), "d")),
      (Sum(Else(Else("a", "b"), "c")),                  Sum(Else("a", Else("b", "c")))),
      (Divide(Else(Else("a", "b"), "c"), "d"),          Divide(Else("a", Else("b", "c")), "d")),
      // format: on
    )

    forAll(table) { (expression, expected) =>
      expression.rewrite shouldBe expected
    }
  }

  test("ValueParser should fail parse unclosed parenthesis") {
    val res = ValueParser.validate("${name")
    res.left.value should be(UnexpectedState("""|Unable to parse expression ${name.
                                                |Errors:
                                                |'}' expected but end of source found""".stripMargin))
  }

  val enrolmentOutcome = EnrolmentOutcome(toSmartString("title"), toSmartString("content"))

  val plainFormTemplate = FormTemplate(
    FormTemplateId("ipt100"),
    FormTemplateId("IPT100"),
    FormTemplateVersion(1),
    Some(
      NonEmptyList.of(
        FormTemplateId("ipt099")
      )
    ),
    toLocalisedString("Insurance Premium Tax Return"),
    Some(BetaBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    None,
    Destinations
      .DestinationList(
        NonEmptyList.of(
          HmrcDms(
            DestinationId("TestHmrcDmsId"),
            Dms,
            "TestHmrcDmsFormId",
            Constant("TestHmrcDmsCustomerId"),
            "TestHmrcDmsClassificationType",
            "TestHmrcDmsBusinessArea",
            DestinationIncludeIf.HandlebarValue(""),
            true,
            Some(DataOutputFormat.XML),
            true,
            Some(true),
            Some(InstructionPdfFields.Ordered),
            None,
            None,
            TemplateType.XML,
            None,
            None,
            None,
            None
          )
        ),
        ackSection,
        Some(
          DeclarationSection(
            toSmartString("Declaration"),
            None,
            None,
            None,
            None,
            Some(toSmartString("ContinueLabel")),
            Nil,
            None
          )
        )
      ),
    HmrcAgentWithEnrolmentModule(
      RequireMTDAgentEnrolment,
      EnrolmentAuth(
        ServiceId("TEST"),
        DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("TEST"))),
        EnrolmentOutcomes(enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome)
      )
    ),
    Some(LocalisedEmailTemplateId("test-email-template-id-en", Some("test-email-template-id-cy"))),
    Some(
      NonEmptyList.of(
        EmailParameter("fullName", FormCtx("directorFullName")),
        EmailParameter("email", FormCtx("directorEmail"))
      )
    ),
    None,
    FormKind.Classic(List.empty[Section]),
    Nil,
    AvailableLanguages.default,
    None,
    SummarySection(
      toSmartString("Title"),
      Some(toSmartString("Caption")),
      toSmartString("Header"),
      toSmartString("Footer"),
      Some(toSmartString("ContinueLabel")),
      None,
      LayoutDisplayWidth.M,
      KeyDisplayWidth.S,
      None,
      None,
      None,
      None,
      None
    ),
    None,
    true,
    FileInfoConfig.allAllowedFileTypes,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    false,
    false,
    None,
    true,
    None,
    Option.empty[CustomSubmissionRef]
  )

  val yourDetailsSection = Section.NonRepeatingPage(
    Page(
      toSmartString("Your details"),
      None,
      None,
      None,
      None,
      None,
      None,
      List(
        FormComponent(
          FormComponentId("firstName"),
          Text(ShortText.default, Value),
          toSmartString("Your first name"),
          false,
          None,
          None,
          includeIf = None,
          validIf = None,
          mandatory = Mandatory.False,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        ),
        FormComponent(
          FormComponentId("lastName"),
          Text(ShortText.default, Value),
          toSmartString("Your last name"),
          false,
          None,
          None,
          includeIf = None,
          validIf = None,
          mandatory = Mandatory.False,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        )
      ),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  )

  val formTemplateWithOneSection = plainFormTemplate.copy(formKind = FormKind.Classic(List(yourDetailsSection)))

  test("Expr.validate should return Valid if expression include fieldName id present in the form template") {

    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("firstName"))), formTemplateWithOneSection)
    res should be(Valid)
  }

  test("Expr.validate should return Valid if expression Add fields present in the form template") {
    val res =
      FormTemplateValidator
        .validate(
          List(Text(ShortText.default, Add(FormCtx("firstName"), FormCtx("lastName")))),
          formTemplateWithOneSection
        )
    res should be(Valid)
  }

  test("Expr.validate should return Valid if expression Multiply fields present in the form template") {
    val res =
      FormTemplateValidator
        .validate(
          List(Text(ShortText.default, Multiply(FormCtx("firstName"), FormCtx("lastName")))),
          formTemplateWithOneSection
        )
    res should be(Valid)
  }

  test(
    "Expr.validate should return Invalid if expression include fieldName id not present in the form template"
  ) {
    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("firstNameTypo"))), formTemplateWithOneSection)
    res should be(Invalid("Form field 'firstNameTypo' is not defined in form template."))
  }
  test("Expr.validate should return invalid and not be parsed as empty string") {
    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("'' * ''"))), formTemplateWithOneSection)
    res should be(Invalid("Form field ''' * ''' is not defined in form template."))
  }

  test("Parser - contextField should parse form field with date offset as DateCtx (form.)") {
    val result = ValueParser.parseAll(ValueParser.contextField, "form.dateField + 1d").get
    result shouldBe DateCtx(
      DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("dateField"))), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  test("Parser - contextField should parse form field with date offset as DateCtx") {
    val result = ValueParser.parseAll(ValueParser.contextField, "dateField + 1d").get
    result shouldBe DateCtx(
      DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("dateField"))), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  test("Parser - contextField should parse form.startDate as DateCtx") {
    val result = ValueParser.parseAll(ValueParser.contextField, "form.startDate").get
    result shouldBe DateCtx(DateValueExpr(FormStartDateExprValue))
  }

  test("Parser - contextField should parse TODAY as DateCtx") {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY").get
    result shouldBe DateCtx(DateValueExpr(TodayDateExprValue))
  }

  test("Parser - contextField should parse TODAY with offset as DateCtx") {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY + 1m").get
    result shouldBe DateCtx(DateExprWithOffset(DateValueExpr(TodayDateExprValue), OffsetYMD(OffsetUnit.Month(1))))
  }

  test("Parser - contextField should parse TODAY with offset as DateCtx y m d") {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY + 2y + 3m + 4d").get
    result shouldBe DateCtx(
      DateExprWithOffset(
        DateValueExpr(TodayDateExprValue),
        OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3), OffsetUnit.Day(4))
      )
    )
  }

  test("Parser - contextField should parse fixed date string as DateCtx") {
    val result = ValueParser.parseAll(ValueParser.contextField, "01012020").get
    result shouldBe DateCtx(DateValueExpr(ExactDateExprValue(2020, 1, 1)))
  }

  test("Parser - contextField should support year/month/day offset for dates") {
    val today = DateValueExpr(TodayDateExprValue)
    val fieldReference = DateFormCtxVar(FormCtx("dateField"))
    val table = Table(
      ("expression", "dateExpr", "offset"),
      ("TODAY + 2y + 3m + 4d", today, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3), OffsetUnit.Day(4))),
      ("TODAY + 2y + 3d + 4m", today, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Day(3), OffsetUnit.Month(4))),
      ("TODAY + 2d + 3m + 4y", today, OffsetYMD(OffsetUnit.Day(2), OffsetUnit.Month(3), OffsetUnit.Year(4))),
      ("TODAY + 2d + 3y + 4m", today, OffsetYMD(OffsetUnit.Day(2), OffsetUnit.Year(3), OffsetUnit.Month(4))),
      ("TODAY + 2m + 3d + 4y", today, OffsetYMD(OffsetUnit.Month(2), OffsetUnit.Day(3), OffsetUnit.Year(4))),
      ("TODAY + 2m + 3y + 4d", today, OffsetYMD(OffsetUnit.Month(2), OffsetUnit.Year(3), OffsetUnit.Day(4))),
      ("TODAY + 2y + 3m", today, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3))),
      ("TODAY + 2m + 3y", today, OffsetYMD(OffsetUnit.Month(2), OffsetUnit.Year(3))),
      ("TODAY + 2y + 4d", today, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Day(4))),
      ("TODAY + 2d + 4y", today, OffsetYMD(OffsetUnit.Day(2), OffsetUnit.Year(4))),
      ("TODAY + 3m + 4d", today, OffsetYMD(OffsetUnit.Month(3), OffsetUnit.Day(4))),
      ("TODAY + 3d + 4m", today, OffsetYMD(OffsetUnit.Day(3), OffsetUnit.Month(4))),
      ("TODAY + 2y", today, OffsetYMD(OffsetUnit.Year(2))),
      ("TODAY + 2m", today, OffsetYMD(OffsetUnit.Month(2))),
      ("TODAY + 2d", today, OffsetYMD(OffsetUnit.Day(2))),
      ("TODAY - 2y - 3m - 4d", today, OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Month(-3), OffsetUnit.Day(-4))),
      ("TODAY - 2y - 3m", today, OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Month(-3))),
      ("TODAY - 2y - 4d", today, OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Day(-4))),
      ("TODAY - 3m - 4d", today, OffsetYMD(OffsetUnit.Month(-3), OffsetUnit.Day(-4))),
      ("TODAY - 2y", today, OffsetYMD(OffsetUnit.Year(-2))),
      ("TODAY - 2m", today, OffsetYMD(OffsetUnit.Month(-2))),
      ("TODAY - 2d", today, OffsetYMD(OffsetUnit.Day(-2))),
      (
        "dateField + 2y + 3m + 4d",
        fieldReference,
        OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3), OffsetUnit.Day(4))
      ),
      ("dateField + 2y + 3m", fieldReference, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3))),
      ("dateField + 2y + 4d", fieldReference, OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Day(4))),
      ("dateField + 3m + 4d", fieldReference, OffsetYMD(OffsetUnit.Month(3), OffsetUnit.Day(4))),
      ("dateField + 2y", fieldReference, OffsetYMD(OffsetUnit.Year(2))),
      ("dateField + 2m", fieldReference, OffsetYMD(OffsetUnit.Month(2))),
      ("dateField + 2d", fieldReference, OffsetYMD(OffsetUnit.Day(2))),
      (
        "dateField - 2y - 3m - 4d",
        fieldReference,
        OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Month(-3), OffsetUnit.Day(-4))
      ),
      ("dateField - 2y - 3m", fieldReference, OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Month(-3))),
      ("dateField - 2y - 4d", fieldReference, OffsetYMD(OffsetUnit.Year(-2), OffsetUnit.Day(-4))),
      ("dateField - 3m - 4d", fieldReference, OffsetYMD(OffsetUnit.Month(-3), OffsetUnit.Day(-4))),
      ("dateField - 2y", fieldReference, OffsetYMD(OffsetUnit.Year(-2))),
      ("dateField - 2m", fieldReference, OffsetYMD(OffsetUnit.Month(-2))),
      ("dateField - 2d", fieldReference, OffsetYMD(OffsetUnit.Day(-2)))
    )

    def expected(dateExpr: DateExpr, offset: OffsetYMD) = TextExpression(DateCtx(DateExprWithOffset(dateExpr, offset)))

    forAll(table) { (expression, dateExpr, offset) =>
      ValueParser.validate("${" + expression + "}") shouldBe Right(expected(dateExpr, offset))
    }
  }

  test("Parser - contextField should not support repeated year/month/day offset") {
    val table = Table(
      "expression",
      "TODAY + 2y + 3m + 4m",
      "TODAY + 2y + 3y",
      "TODAY + 2m + 3m",
      "TODAY + 2d + 3d"
    )

    forAll(table) { expression =>
      ValueParser.validate("${" + expression + "}").isLeft shouldBe true
    }
  }

  test("Parser - contextField should parse day(TODAY) function") {
    val res = ValueParser.validate("${day(TODAY)}")
    res.toOption.value should be(TextExpression(DateFunction(DateProjection.Day(DateValueExpr(TodayDateExprValue)))))
  }
  test("Parser - contextField should parse year(01012020) function") {
    val res = ValueParser.validate("${year(01012020)}")
    res.toOption.value should be(
      TextExpression(DateFunction(DateProjection.Year(DateValueExpr(ExactDateExprValue(2020, 1, 1)))))
    )
  }

  test("Parser - contextField should parse month(dateField + 1d) function") {
    val res = ValueParser.validate("${month(dateField + 1d)}")
    res.toOption.value should be(
      TextExpression(
        DateFunction(
          DateProjection.Month(
            DateExprWithOffset(DateFormCtxVar(FormCtx("dateField")), OffsetYMD(List(OffsetUnit.Day(1))))
          )
        )
      )
    )
  }

  test("Parser - contextField should parse tax year from form start date") {
    val res = ValueParser.validate("${taxYear(form.startDate)}")
    res.toOption.value should be(
      TextExpression(DateFunction(DateProjection.TaxYear(DateValueExpr(FormStartDateExprValue))))
    )
  }

  test("Parser - contextField should parse tax year from today's date") {
    val res = ValueParser.validate("${taxYear(TODAY)}")
    res.toOption.value should be(
      TextExpression(DateFunction(DateProjection.TaxYear(DateValueExpr(TodayDateExprValue))))
    )
  }

  test("Parser - contextField should parse yearToDate('0102', TODAY) function") {
    val res = ValueParser.validate("${yearToDate('0102', TODAY)}")

    res.toOption.value should be(
      TextExpression(
        DateCtx(
          DateConstructExpr(
            DateValueExpr(ExactDateExprValue(1900, 2, 1)),
            DateCtx(DateValueExpr(TodayDateExprValue))
          )
        )
      )
    )
  }

  test("Parser - contextField should parse yearToDate('0102', formField) function") {
    val res = ValueParser.validate("${yearToDate('0102', exprId)}")

    res.toOption.value should be(
      TextExpression(
        DateCtx(
          DateConstructExpr(
            DateValueExpr(ExactDateExprValue(1900, 2, 1)),
            FormCtx(FormComponentId("exprId"))
          )
        )
      )
    )
  }

  test("Parser - contextField should parse period(d1, d2) function") {
    val res = ValueParser.validate("${period(d1, d2)}")
    res.toOption.value should be(
      TextExpression(
        Period(
          DateCtx(DateFormCtxVar(FormCtx("d1"))),
          DateCtx(DateFormCtxVar(FormCtx("d2"))),
          PeriodFn.Identity
        )
      )
    )
  }

  test("Parser - contextField should parse period(d1, d2) extension functions") {
    val table = Table(
      ("function", "expected prop"),
      ("${period(d1, d2).sum}", PeriodFn.Sum),
      ("${period(d1, d2).totalMonths}", PeriodFn.TotalMonths),
      ("${period(d1, d2).years}", PeriodFn.Years),
      ("${period(d1, d2).months}", PeriodFn.Months),
      ("${period(d1, d2).days}", PeriodFn.Days)
    )
    forAll(table) { (f: String, expectedFn: PeriodFn) =>
      val res = ValueParser.validate(f)
      res.toOption.value should be(
        TextExpression(
          Period(
            DateCtx(DateFormCtxVar(FormCtx("d1"))),
            DateCtx(DateFormCtxVar(FormCtx("d2"))),
            expectedFn
          )
        )
      )
    }
  }

  test("Parser - contextField should parse days/weeks between functions") {
    val table = Table(
      ("function", "expected prop"),
      ("${daysBetween(d1, d2)}", MeasurementType.Days),
      ("${weeksBetween(d1, d2)}", MeasurementType.Weeks)
    )
    forAll(table) { (f: String, expectedFn: MeasurementType) =>
      val res = ValueParser.validate(f)
      res.toOption.value should be(
        TextExpression(
          Between(
            DateCtx(DateFormCtxVar(FormCtx("d1"))),
            DateCtx(DateFormCtxVar(FormCtx("d2"))),
            expectedFn
          )
        )
      )
    }
  }

  test("Parser - contextField should parse days/weeks between sum functions") {
    val table = Table(
      ("function", "expected prop"),
      ("${daysBetween(d1, d2).sum}", MeasurementType.Days),
      ("${weeksBetween(d1, d2).sum}", MeasurementType.Weeks)
    )
    forAll(table) { (f: String, expectedFn: MeasurementType) =>
      val res = ValueParser.validate(f)
      res.toOption.value should be(
        TextExpression(
          Sum(
            Between(
              DateCtx(DateFormCtxVar(FormCtx("d1"))),
              DateCtx(DateFormCtxVar(FormCtx("d2"))),
              expectedFn
            )
          )
        )
      )
    }
  }

  test("Parser - contextField should parse form.lang as LangCtx") {
    ValueParser.validate("${form.lang}") shouldBe Right(TextExpression(LangCtx))
  }

  test("Parser - contextField should parse link.pageid as PageLink(PageId(pageid))") {
    ValueParser.validate("${link.pageid}") shouldBe Right(TextExpression(LinkCtx(PageLink(PageId("pageid")))))
  }

  test("Parser - contextField should parse link.newForm as PageLink(NewForm)") {
    ValueParser.validate("${link.newForm}") shouldBe Right(TextExpression(LinkCtx(NewForm)))
  }

  test("Parser - contextField should parse link.newForm.xxxx as PageLink(NewForm(xxxx))") {
    ValueParser.validate("${link.newForm.abc-def_123}") shouldBe Right(
      TextExpression(LinkCtx(NewFormForTemplate(FormTemplateId("abc-def_123"))))
    )
  }

  test("Parser - contextField should parse link.newSession as PageLink(NewSession)") {
    ValueParser.validate("${link.newSession}") shouldBe Right(TextExpression(LinkCtx(NewSession)))
  }

  test("Parser - contextField should parse link.signOut as PageLink(SignOut)") {
    ValueParser.validate("${link.signOut}") shouldBe Right(TextExpression(LinkCtx(SignOut)))
  }

  test("Parser - contextField should parse link.printSectionPdf as PageLink(PrintSectionPdf)") {
    ValueParser.validate("${link.printSectionPdf}") shouldBe Right(TextExpression(LinkCtx(PrintSectionPdf)))
  }

  test(
    "Parser - contextField should parse link.printSectionNotificationPdf as PageLink(PrintSectionNotificationPdf)"
  ) {
    ValueParser.validate("${link.printSectionNotificationPdf}") shouldBe Right(
      TextExpression(LinkCtx(PrintSectionNotificationPdf))
    )
  }

  test(
    "Parser - contextField should parse link.printAcknowledgementHtml as PageLink(PrintAcknowledgementHtml)"
  ) {
    ValueParser.validate("${link.printAcknowledgementHtml}") shouldBe Right(
      TextExpression(LinkCtx(PrintAcknowledgementHtml))
    )
  }

  test("Parser - contextField should parse ${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.xxx} as UserCtx") {
    val table = Table(
      ("expr", "result"),
      (
        "${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.count}",
        Right(
          TextExpression(
            UserCtx(
              Enrolment(ServiceName("HMCE-VATDEC-ORG"), IdentifierName("VATRegNo"), Some(UserFieldFunc.Count))
            )
          )
        )
      ),
      (
        "${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.1}",
        Right(
          TextExpression(
            UserCtx(
              Enrolment(ServiceName("HMCE-VATDEC-ORG"), IdentifierName("VATRegNo"), Some(UserFieldFunc.Index(1)))
            )
          )
        )
      ),
      (
        "${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.11}",
        Right(
          TextExpression(
            UserCtx(
              Enrolment(ServiceName("HMCE-VATDEC-ORG"), IdentifierName("VATRegNo"), Some(UserFieldFunc.Index(11)))
            )
          )
        )
      ),
      (
        "${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.0}",
        Left(
          UnexpectedState(
            """Unable to parse expression ${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.0}.
              |Errors:
              |'}' expected but '.' found""".stripMargin
          )
        )
      ),
      (
        "${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.unknown}",
        Left(
          UnexpectedState(
            """Unable to parse expression ${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.unknown}.
              |Errors:
              |'}' expected but '.' found""".stripMargin
          )
        )
      )
    )
    forAll(table) { (expr, expected) =>
      ValueParser.validate(expr) shouldBe expected
    }
  }

  test("Parser - contextField should parse business bank existence - accountNumberIsWellFormatted") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.accountNumberIsWellFormatted}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("accountNumberIsWellFormatted"))
      )
    )
  }

  test("Parser - contextField should parse business bank existence - sortCodeIsPresentOnEISCD") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeIsPresentOnEISCD}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"))
      )
    )
  }

  test("Parser - contextField should parse business bank existence - sortCodeBankName") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeBankName}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeBankName")))
    )
  }

  test(
    "Parser - contextField should parse business bank existence - nonStandardAccountDetailsRequiredForBacs"
  ) {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.nonStandardAccountDetailsRequiredForBacs}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(
          DataRetrieveId("businessBankDetails"),
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs")
        )
      )
    )
  }

  test("Parser - contextField should parse business bank existence - accountExists") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.accountExists}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("accountExists")))
    )
  }

  test("Parser - contextField should parse business bank existence - nameMatches") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.nameMatches}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("nameMatches")))
    )
  }

  test("Parser - contextField should parse business bank existence - sortCodeSupportsDirectDebit") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeSupportsDirectDebit}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeSupportsDirectDebit"))
      )
    )
  }

  test("Parser - contextField should parse business bank existence - sortCodeSupportsDirectCredit") {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeSupportsDirectCredit}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeSupportsDirectCredit"))
      )
    )
  }

  test("Parser - contextField should parse quotedLocalisedConstant localized constant") {
    val res = ValueParser.parseAll(ValueParser.quotedLocalisedConstant, "'Are you'| 'Wyt ti'")
    res.get shouldBe IfElse(Equals(LangCtx, Constant("en")), Constant("Are you"), Constant("Wyt ti"))

  }

  test("Parser - contextField should parse complex if-then-else expression from acceptance tests") {

    val res = ValueParser.validate("${if noCompanyName then 'Are you'| 'Wyt ti' else ('Is '|'Os ' + companyName)}")

    res.toOption.value shouldBe TextExpression(
      IfElse(
        TopLevelRef(BooleanExprId("noCompanyName")),
        IfElse(Equals(LangCtx, Constant("en")), Constant("Are you"), Constant("Wyt ti")),
        Add(IfElse(Equals(LangCtx, Constant("en")), Constant("Is "), Constant("Os ")), FormCtx("companyName"))
      )
    )

  }

  test("Parser - contextField should parse hmrcTaxPeriodCtx as DateCtx") {
    val res = ValueParser.validate("${returnTaxPeriod.periodTo}")
    res.toOption.value shouldBe TextExpression(
      DateCtx(
        HmrcTaxPeriodCtx(FormCtx(FormComponentId("returnTaxPeriod")), HmrcTaxPeriodInfo.PeriodTo)
      )
    )
  }

  test("Parser - contextField should parse selectedCountry.column.InEU") {
    val res = ValueParser.validate("${selectedCountry.column.InEU}")
    res.toOption.value should be(TextExpression(LookupColumn(FormComponentId("selectedCountry"), "InEU")))
  }

  test("Parser - contextField should parse applicantNationality.column.InEU.count('1')") {
    val res = ValueParser.validate("${applicantNationality.column.InEU.count('1')}")
    res.toOption.value should be(
      TextExpression(CsvCountryCountCheck(FormComponentId("applicantNationality"), "InEU", "1"))
    )
  }

  test("Parser - contextField should parse applicantNationality.column.Name.count('United Kingdom')") {
    val res = ValueParser.validate("${applicantNationality.column.Name.count('United Kingdom')}")
    res.toOption.value should be(
      TextExpression(CsvCountryCountCheck(FormComponentId("applicantNationality"), "Name", "United Kingdom"))
    )
  }

  test("Parser - contextField should parse company house profile - name") {
    val res = ValueParser.validate("${dataRetrieve.company.name}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("company"), DataRetrieve.Attribute("name"))
      )
    )
  }

  test("Parser - contextField should parse company house profile - status") {
    val res = ValueParser.validate("${dataRetrieve.company.status}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("company"), DataRetrieve.Attribute("status"))
      )
    )
  }

  test("Parser - contextField should parse company house profile - registeredAddress") {
    val res = ValueParser.validate("${dataRetrieve.company.registeredAddress}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("company"), DataRetrieve.Attribute("registeredAddress"))
      )
    )
  }

  test("Parser - contextField should parse company house officers - activeDirectors") {
    val res = ValueParser.validate("${dataRetrieve.directors.activeDirectors}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("directors"), DataRetrieve.Attribute("activeDirectors"))
      )
    )
  }

  test("Parser - contextField should parse company house officers - activeSecretaries") {
    val res = ValueParser.validate("${dataRetrieve.directors.activeSecretaries}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("directors"), DataRetrieve.Attribute("activeSecretaries"))
      )
    )
  }

  test("Parser - contextField should parse company house officers - activeLlpMembers") {
    val res = ValueParser.validate("${dataRetrieve.directors.activeLlpMembers}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("directors"), DataRetrieve.Attribute("activeLlpMembers"))
      )
    )
  }

  test("Parser - contextField should parse nino insights - riskScore") {
    val res = ValueParser.validate("${dataRetrieve.ninoInsights.riskScore}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("ninoInsights"), DataRetrieve.Attribute("riskScore"))
      )
    )
  }

  test("Parser - contextField should parse nino insights - reason") {
    val res = ValueParser.validate("${dataRetrieve.ninoInsights.reason}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("ninoInsights"), DataRetrieve.Attribute("reason"))
      )
    )
  }

  test("Parser - contextField should parse bank account insights - riskScore") {
    val res = ValueParser.validate("${dataRetrieve.bankCheck.riskScore}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("bankCheck"), DataRetrieve.Attribute("riskScore"))
      )
    )
  }

  test("Parser - contextField should parse bank account insights - reason") {
    val res = ValueParser.validate("${dataRetrieve.bankCheck.reason}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("bankCheck"), DataRetrieve.Attribute("reason"))
      )
    )
  }

  test("Parser - contextField should parse personal bank account existence - accountExists") {
    val res = ValueParser.validate("${dataRetrieve.personalBankAccountExistence.accountExists}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("personalBankAccountExistence"), DataRetrieve.Attribute("accountExists"))
      )
    )
  }

  test("Parser - contextField should parse personal bank account existence - nameMatches") {
    val res = ValueParser.validate("${dataRetrieve.personalBankAccountExistence.nameMatches}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("personalBankAccountExistence"), DataRetrieve.Attribute("nameMatches"))
      )
    )
  }

  test("Parser - contextField should parse substring function") {
    val res = ValueParser.validate("${substring(fieldId, 3, 4)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.SubString(3, 4)))
    )
  }

  test("Parser - contextField should parse removeSpaces function") {
    val res = ValueParser.validate("${removeSpaces(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.RemoveSpaces))
    )
  }

  test("Parser - contextField should parse capitalize function") {
    val res = ValueParser.validate("${capitalize(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.Capitalize))
    )
  }

  test("Parser - contextField should parse capitalizeAll function") {
    val res = ValueParser.validate("${capitalizeAll(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.CapitalizeAll))
    )
  }

  test("Parser - contextField should parse uppercase function") {
    val res = ValueParser.validate("${uppercase(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.UpperCase))
    )
  }

  test("Parser - contextField should parse lowercase function") {
    val res = ValueParser.validate("${lowercase(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.LowerCase))
    )
  }

  test("Parser - contextField should parse lowercaseFirst function") {
    val res = ValueParser.validate("${lowercaseFirst(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.LowerCaseFirst))
    )
  }

  test("Parser - contextField should parse lowercase, uppercase, and capitalize functions") {
    val res = ValueParser.validate("${capitalize(uppercase(lowercase(fieldId)))}")
    res.toOption.value should be(
      TextExpression(
        StringOps(
          StringOps(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.LowerCase), StringFnc.UpperCase),
          StringFnc.Capitalize
        )
      )
    )
  }

  test("Parser - contextField should parse concat function") {
    val res = ValueParser.validate("${concat(fieldId, ' test')}")
    res.toOption.value should be(
      TextExpression(Concat(List(FormCtx(FormComponentId("fieldId")), Constant(" test"))))
    )
  }

  test("Parser - contextField should parse auth.itmpAddress.country function") {
    val res = ValueParser.validate("${auth.itmpAddress.country}")
    res.toOption.value should be(
      TextExpression(CountryOfItmpAddress)
    )
  }

  test("Parser - contextField should parse link.download.nipClaimScheduleTemplate.xlsx function") {
    val res = ValueParser.validate("${link.download.nipClaimScheduleTemplate.xlsx}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Download("nipClaimScheduleTemplate.xlsx")))
    )
  }

  test("Parser - contextField should parse link.download.nipClaimScheduleTemplate.ods function") {
    val res = ValueParser.validate("${link.download.nipClaimScheduleTemplate.ods}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Download("nipClaimScheduleTemplate.ods")))
    )
  }

  test("Parser - contextField should parse link.image.imagename.png function") {
    val res = ValueParser.validate("${link.image.imagename.png}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Image("imagename.png")))
    )
  }

  test("Parser - contextField should parse choicesRevealedField function") {
    val res = ValueParser.validate("${choicesRevealedField(fieldId)}")
    res.toOption.value should be(
      TextExpression(ChoicesRevealedField(FormComponentId("fieldId")))
    )
  }

  test("Parser - contextField should parse link.'https://www.gov.uk' function") {
    val res = ValueParser.validate("${link.'https://www.gov.uk'}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.UrlLink("https://www.gov.uk")))
    )
  }

  test("Parser - contextField should parse choice function") {
    val res = ValueParser.validate("${choice(fieldId)}")
    res.toOption.value should be(
      TextExpression(Concat(List(FormCtx(FormComponentId("fieldId")))))
    )
  }

  test("Parser - contextField should parse the hideZeroDecimals function") {
    val res = ValueParser.validate("${hideZeroDecimals(age)}")

    res.toOption.value should be(TextExpression(HideZeroDecimals(FormCtx("age"))))
  }

  test("Parser - contextField should parse the hideZeroDecimals function with other functions") {
    val res = ValueParser.validate("${hideZeroDecimals(age + 2)}")

    res.toOption.value should be(TextExpression(HideZeroDecimals(Add(FormCtx("age"), Constant("2")))))
  }

  test("Parser - contextField should parse substring function with user field enrolments") {
    val res = ValueParser.validate("${substring(user.enrolments.HMRC-CUS-ORG.EORINumber,0,2)}")
    res.toOption.value should be(
      TextExpression(
        StringOps(
          UserCtx(Enrolment(ServiceName("HMRC-CUS-ORG"), IdentifierName("EORINumber"), None)),
          StringFnc.SubString(0, 2)
        )
      )
    )
  }

  test("Parser - contextField should parse the getCountry function") {
    val res = ValueParser.validate("${getCountry(age)}")

    res.toOption.value should be(TextExpression(LookupOps(FormCtx("age"), LookupFnc.CountryName)))
  }

  test("Parser - contextField should parse the getSicDescription function") {
    val res = ValueParser.validate("${getSicDescription(age)}")

    res.toOption.value should be(TextExpression(LookupOps(FormCtx("age"), LookupFnc.SicDescription)))
  }

}
