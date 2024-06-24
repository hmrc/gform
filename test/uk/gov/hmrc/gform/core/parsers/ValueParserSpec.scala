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

import cats.data.NonEmptyList
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.{ NewForm, NewFormForTemplate, NewSession, PageLink, SignOut }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField.Enrolment
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, DestinationId, DestinationIncludeIf, Destinations, TemplateType }

import scala.language.implicitConversions

class ValueParserSpec extends Spec with TableDrivenPropertyChecks {

  implicit def implicitToFormComponentId(str: String): FormComponentId = FormComponentId(str)

  "ValueParser" should "parse integer" in {
    val res = ValueParser.validate("${1}")
    res.toOption.value should be(TextExpression(Constant("1")))
  }

  it should "parse constant" in {
    val res = ValueParser.validate("${' '}")
    res.toOption.value should be(TextExpression(Constant(" ")))
  }

  it should "parse integer with decimal point" in {
    val res = ValueParser.validate("${1.}")
    res.toOption.value should be(TextExpression(Constant("1.")))
  }

  it should "parse multi digit integer" in {
    val res = ValueParser.validate("${12}")
    res.toOption.value should be(TextExpression(Constant("12")))
  }
  it should "parse an empty string" in {
    val res = ValueParser.validate("${''}")
    res.toOption.value should be(TextExpression(Constant("")))
  }
  it should "parse an anything except singe quote" in {
    val res = ValueParser.validate("${' --+===> '}")
    res.toOption.value should be(TextExpression(Constant(" --+===> ")))
  }
  it should "parse double digit integer" in {
    val res = ValueParser.validate("${1234}")
    res.toOption.value should be(TextExpression(Constant("1234")))
  }

  it should "parse multi digit integer with thousand separators" in {
    val res = ValueParser.validate("${1,234}")
    res.toOption.value should be(TextExpression(Constant("1,234")))
  }

  it should "parse double digit decimal fraction" in {
    val res = ValueParser.validate("${.12}")
    res.toOption.value should be(TextExpression(Constant(".12")))
  }

  it should "parse multi digit decimal fraction" in {
    val res = ValueParser.validate("${.123}")
    res.toOption.value should be(TextExpression(Constant(".123")))
  }

  it should "parse number plus decimal fraction" in {
    val res = ValueParser.validate("${1,234.1}")
    res.toOption.value should be(TextExpression(Constant("1,234.1")))
  }

  it should "parse number plus double digit decimal fraction" in {
    val res = ValueParser.validate("${1,234.12}")
    res.toOption.value should be(TextExpression(Constant("1,234.12")))
  }

  it should "parse number plus multi digit decimal fraction" in {
    val res = ValueParser.validate("${1,234.123}")
    res.toOption.value should be(TextExpression(Constant("1,234.123")))
  }

  it should "parse ${firstName}" in {
    val res = ValueParser.validate("${firstName}")
    res.toOption.value should be(TextExpression(FormCtx("firstName")))
  }

  it should "parse ${age - 1}" in {
    val res = ValueParser.validate("${age - 1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${age -1}" in {
    val res = ValueParser.validate("${age -1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${age-1}" in {
    val res = ValueParser.validate("${age-1}")
    res.toOption.value should be(TextExpression(Subtraction(FormCtx("age"), Constant("1"))))
  }

  it should "parse ${form.firstName}" in {
    val res = ValueParser.validate("${form.firstName}")
    res.toOption.value should be(TextExpression(FormCtx("firstName")))
  }

  it should "parse ${user.enrolledIdentifier}" in {
    val res = ValueParser.validate("${user.enrolledIdentifier}")
    res.toOption.value should be(TextExpression(UserCtx(UserField.EnrolledIdentifier)))
  }

  it should "parse anything except singe quote" in {
    val res = ValueParser.validate("${' --+===>., '}")
    res.toOption.value should be(TextExpression(Constant(" --+===>., ")))
  }

  it should "fail to parse ${user.enrolledIdentifier" in {

    val res = ValueParser.validate("${user.enrolledIdentifier")
    res.left.value should be(
      UnexpectedState("""Unable to parse expression ${user.enrolledIdentifier.
                        |Errors:
                        |'}' expected but end of source found""".stripMargin)
    )
  }

  it should "fail to parse ${user.enrolledIdentifiers}" in {

    val res = ValueParser.validate("${user.enrolledIdentifiers}")
    res.left.value should be(
      UnexpectedState(
        """Unable to parse expression ${user.enrolledIdentifiers}.
          |Errors:
          |'}' expected but 's' found""".stripMargin
      )
    )
  }

  it should "parse ${user.credentialRole}" in {
    val res = ValueParser.validate("${user.credentialRole}")
    res.toOption.value should be(TextExpression(UserCtx(UserField.CredentialRole)))
  }

  it should "parse ${user.enrolments.<identifierName>.<referenceName>}" in {

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

  it should "parse ${someId.sum}" in {
    val res = ValueParser.validate("${age.sum}")

    res.toOption.value should be(TextExpression(Sum(FormCtx("age"))))
  }

  it should "parse ${firstName * secondName}" in {
    val res = ValueParser.validate("${firstName * secondName}")
    res.toOption.value should be(TextExpression(Multiply(FormCtx("firstName"), FormCtx("secondName"))))
  }

  it should "parse ${firstName * auth.secondName}" in {
    val res = ValueParser.validate("${firstName * auth.sautr}")
    res.toOption.value should be(TextExpression(Multiply(FormCtx("firstName"), AuthCtx(AuthInfo.SaUtr))))
  }

  it should "parse ${auth.email}" in {
    val res = ValueParser.validate("${auth.email}")
    res.toOption.value should be(TextExpression(AuthCtx(AuthInfo.EmailId)))
  }

  it should "parse ${a - b  * c}" in {
    val res = ValueParser.validate("${firstName - secondName * thirdname}")
    res.toOption.value should be(
      TextExpression(Subtraction(FormCtx("firstName"), Multiply(FormCtx("secondName"), FormCtx("thirdname"))))
    )
  }

  it should "parse ${a * b  + c}" in {
    val res = ValueParser.validate("${firstName * secondName + thirdname}")
    res.toOption.value should be(
      TextExpression(Add(Multiply(FormCtx("firstName"), FormCtx("secondName")), FormCtx("thirdname")))
    )
  }

  it should "parse ${a / b}" in {
    val res = ValueParser.validate("${firstName / secondName}")
    res.toOption.value should be(
      TextExpression(Divide(FormCtx("firstName"), FormCtx("secondName")))
    )
  }

  it should "parse ${a * b  / c}" in {
    val res = ValueParser.validate("${firstName * secondName / thirdname}")
    res.toOption.value should be(
      TextExpression(Divide(Multiply(FormCtx("firstName"), FormCtx("secondName")), FormCtx("thirdname")))
    )
  }

  it should "parse string constant" in {
    val res = ValueParser.validate("'constant'")
    res.toOption.value should be(TextExpression(Constant("constant")))
  }

  it should "parse string constant including space" in {
    val res = ValueParser.validate("'const ant'")
    res.toOption.value should be(TextExpression(Constant("const ant")))
  }

  it should "parse number as a choice selections" in {
    val res = ValueParser.validate("1")
    res.toOption.value should be(ChoiceExpression(List(1)))
  }

  it should "parse numbers separated by comma as a choice selections" in {
    val res = ValueParser.validate("1,2,3,4")
    res.toOption.value should be(ChoiceExpression(List(1, 2, 3, 4)))
  }

  /** Date cases
    */
  it should "parse Date" in {
    val res = ValueParser.validate("2015-01-15")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 15)))
  }

  it should "parse lastDay" in {
    val res = ValueParser.validate("2015-01-lastDay")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 31)))
  }

  it should "parse firstDay" in {
    val res = ValueParser.validate("2015-01-firstDay")
    res.toOption.value should be(DateExpression(ExactDateValue(2015, 1, 1)))
  }

  it should "throw exception on 1 digit month " in {
    val res = ValueParser.validate("2015-1-12")
    res.left.value should be(
      UnexpectedState("""Unable to parse expression 2015-1-12.
                        |Errors:
                        |end of input expected""".stripMargin)
    )
  }

  it should "throw exception on year digits" in {
    val res = ValueParser.validate("201568-01-12")
    res.left.value should be(UnexpectedState("""Unable to parse expression 201568-01-12.
                                               |Errors:
                                               |end of input expected""".stripMargin))
  }

  it should "throw exception on Date format" in {
    val res = ValueParser.validate("65841-351")
    res.left.value should be(UnexpectedState("""Unable to parse expression 65841-351.
                                               |Errors:
                                               |end of input expected""".stripMargin))
  }

  it should "parse next Date setting next year" in {
    val res = ValueParser.validate("next-01-15")

    res.toOption.value should be(DateExpression(NextDateValue(1, 15)))
  }

  it should "parse next Date setting current year" in {
    val res = ValueParser.validate("next-04-15")

    res.toOption.value should be(DateExpression(NextDateValue(4, 15)))
  }

  it should "parse last Date setting current year" in {
    val res = ValueParser.validate("last-01-15")

    res.toOption.value should be(DateExpression(PreviousDateValue(1, 15)))
  }

  it should "parse last Date setting previous year" in {
    val res = ValueParser.validate("last-04-15")

    res.toOption.value should be(DateExpression(PreviousDateValue(4, 15)))
  }

  it should "parse Date setting current Date" in {
    val res = ValueParser.validate("today")

    res.toOption.value should be(DateExpression(TodayDateValue))
  }

  it should "parse submissionReference" in {
    val res = ValueParser.validate("${form.submissionReference}")

    res.toOption.value should be(TextExpression(FormTemplateCtx(FormTemplateProp.SubmissionReference)))
  }

  it should "parse fileSizeLimit" in {
    val res = ValueParser.validate("${form.fileSizeLimit}")

    res.toOption.value should be(TextExpression(FormTemplateCtx(FormTemplateProp.FileSizeLimit)))
  }

  it should "parse orElse expression" in {
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

  it should "else expressions should be rewriten to nesting on right hand side" in {
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

  it should "fail parse unclosed parenthesis" in {
    val res = ValueParser.validate("${name")
    res.left.value should be(UnexpectedState("""|Unable to parse expression ${name.
                                                |Errors:
                                                |'}' expected but end of source found""".stripMargin))
  }

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
    Some(ResearchBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    Destinations
      .DestinationList(
        NonEmptyList.of(
          HmrcDms(
            DestinationId("TestHmrcDmsId"),
            "TestHmrcDmsFormId",
            Constant("TestHmrcDmsCustomerId"),
            "TestHmrcDmsClassificationType",
            "TestHmrcDmsBusinessArea",
            DestinationIncludeIf.HandlebarValue(""),
            true,
            Some(DataOutputFormat.XML),
            true,
            Some(true),
            true,
            None,
            None,
            TemplateType.XML
          )
        ),
        ackSection,
        Some(
          DeclarationSection(
            toSmartString("Declaration"),
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
      EnrolmentAuth(ServiceId("TEST"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("TEST"))))
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
    None
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
          None,
          None,
          includeIf = None,
          validIf = None,
          mandatory = false,
          editable = true,
          submissible = true,
          derived = false,
          errorMessage = None
        ),
        FormComponent(
          FormComponentId("lastName"),
          Text(ShortText.default, Value),
          toSmartString("Your last name"),
          None,
          None,
          includeIf = None,
          validIf = None,
          mandatory = false,
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

  "Expr.validate" should "return Valid if expression include fieldName id present in the form template" in {

    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("firstName"))), formTemplateWithOneSection)
    res should be(Valid)
  }

  it should "return Valid if expression Add fields present in the form template" in {
    val res =
      FormTemplateValidator
        .validate(
          List(Text(ShortText.default, Add(FormCtx("firstName"), FormCtx("lastName")))),
          formTemplateWithOneSection
        )
    res should be(Valid)
  }

  it should "return Valid if expression Multiply fields present in the form template" in {
    val res =
      FormTemplateValidator
        .validate(
          List(Text(ShortText.default, Multiply(FormCtx("firstName"), FormCtx("lastName")))),
          formTemplateWithOneSection
        )
    res should be(Valid)
  }

  it should "return Invalid if expression include fieldName id not present in the form template" in {
    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("firstNameTypo"))), formTemplateWithOneSection)
    res should be(Invalid("Form field 'firstNameTypo' is not defined in form template."))
  }
  it should "return invalid and not be parsed as empty string" in {
    val res = FormTemplateValidator
      .validate(List(Text(ShortText.default, FormCtx("'' * ''"))), formTemplateWithOneSection)
    res should be(Invalid("Form field ''' * ''' is not defined in form template."))
  }

  "Parser - contextField" should "parse form field with date offset as DateCtx (form.)" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "form.dateField + 1d").get
    result shouldBe DateCtx(
      DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("dateField"))), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  it should "parse form field with date offset as DateCtx" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "dateField + 1d").get
    result shouldBe DateCtx(
      DateExprWithOffset(DateFormCtxVar(FormCtx(FormComponentId("dateField"))), OffsetYMD(OffsetUnit.Day(1)))
    )
  }

  it should "parse TODAY as DateCtx" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY").get
    result shouldBe DateCtx(DateValueExpr(TodayDateExprValue))
  }

  it should "parse TODAY with offset as DateCtx" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY + 1m").get
    result shouldBe DateCtx(DateExprWithOffset(DateValueExpr(TodayDateExprValue), OffsetYMD(OffsetUnit.Month(1))))
  }

  it should "parse TODAY with offset as DateCtx y m d" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "TODAY + 2y + 3m + 4d").get
    result shouldBe DateCtx(
      DateExprWithOffset(
        DateValueExpr(TodayDateExprValue),
        OffsetYMD(OffsetUnit.Year(2), OffsetUnit.Month(3), OffsetUnit.Day(4))
      )
    )
  }

  it should "parse fixed date string as DateCtx" in {
    val result = ValueParser.parseAll(ValueParser.contextField, "01012020").get
    result shouldBe DateCtx(DateValueExpr(ExactDateExprValue(2020, 1, 1)))
  }

  it should "support year/month/day offset for dates" in {
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

  it should "not support repeated year/month/day offset" in {
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

  it should "parse day(TODAY) function" in {
    val res = ValueParser.validate("${day(TODAY)}")
    res.toOption.value should be(TextExpression(DateFunction(DateProjection.Day(DateValueExpr(TodayDateExprValue)))))
  }
  it should "parse year(01012020) function" in {
    val res = ValueParser.validate("${year(01012020)}")
    res.toOption.value should be(
      TextExpression(DateFunction(DateProjection.Year(DateValueExpr(ExactDateExprValue(2020, 1, 1)))))
    )
  }

  it should "parse month(dateField + 1d) function" in {
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

  it should "parse period(d1, d2) function" in {
    val res = ValueParser.validate("${period(d1, d2)}")
    res.toOption.value should be(
      TextExpression(
        Period(
          DateCtx(DateFormCtxVar(FormCtx("d1"))),
          DateCtx(DateFormCtxVar(FormCtx("d2")))
        )
      )
    )
  }

  it should "parse period(d1, d2) extension functions" in {
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
          PeriodExt(
            Period(
              DateCtx(DateFormCtxVar(FormCtx("d1"))),
              DateCtx(DateFormCtxVar(FormCtx("d2")))
            ),
            expectedFn
          )
        )
      )
    }
  }

  it should "parse form.lang as LangCtx" in {
    ValueParser.validate("${form.lang}") shouldBe Right(TextExpression(LangCtx))
  }

  it should "parse link.pageid as PageLink(PageId(pageid))" in {
    ValueParser.validate("${link.pageid}") shouldBe Right(TextExpression(LinkCtx(PageLink(PageId("pageid")))))
  }

  it should "parse link.newForm as PageLink(NewForm)" in {
    ValueParser.validate("${link.newForm}") shouldBe Right(TextExpression(LinkCtx(NewForm)))
  }

  it should "parse link.newForm.xxxx as PageLink(NewForm(xxxx))" in {
    ValueParser.validate("${link.newForm.abc-def_123}") shouldBe Right(
      TextExpression(LinkCtx(NewFormForTemplate(FormTemplateId("abc-def_123"))))
    )
  }

  it should "parse link.newSession as PageLink(NewSession)" in {
    ValueParser.validate("${link.newSession}") shouldBe Right(TextExpression(LinkCtx(NewSession)))
  }

  it should "parse link.signOut as PageLink(SignOut)" in {
    ValueParser.validate("${link.signOut}") shouldBe Right(TextExpression(LinkCtx(SignOut)))
  }

  it should "parse ${user.enrolments.HMCE-VATDEC-ORG.VATRegNo.xxx} as UserCtx" in {
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

  it should "parse business bank existence - accountNumberIsWellFormatted" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.accountNumberIsWellFormatted}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("accountNumberIsWellFormatted"))
      )
    )
  }

  it should "parse business bank existence - sortCodeIsPresentOnEISCD" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeIsPresentOnEISCD}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"))
      )
    )
  }

  it should "parse business bank existence - sortCodeBankName" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeBankName}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeBankName")))
    )
  }

  it should "parse business bank existence - nonStandardAccountDetailsRequiredForBacs" in {
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

  it should "parse business bank existence - accountExists" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.accountExists}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("accountExists")))
    )
  }

  it should "parse business bank existence - nameMatches" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.nameMatches}")
    res.toOption.value should be(
      TextExpression(DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("nameMatches")))
    )
  }

  it should "parse business bank existence - sortCodeSupportsDirectDebit" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeSupportsDirectDebit}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeSupportsDirectDebit"))
      )
    )
  }

  it should "parse business bank existence - sortCodeSupportsDirectCredit" in {
    val res = ValueParser.validate("${dataRetrieve.businessBankDetails.sortCodeSupportsDirectCredit}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("businessBankDetails"), DataRetrieve.Attribute("sortCodeSupportsDirectCredit"))
      )
    )
  }

  it should "parse quotedLocalisedConstant localized constant" in {
    val res = ValueParser.parseAll(ValueParser.quotedLocalisedConstant, "'Are you'| 'Wyt ti'")
    res.get shouldBe IfElse(Equals(LangCtx, Constant("en")), Constant("Are you"), Constant("Wyt ti"))

  }

  it should "parse complex if-then-else expression from acceptance tests" in {

    val res = ValueParser.validate("${if noCompanyName then 'Are you'| 'Wyt ti' else ('Is '|'Os ' + companyName)}")

    res.toOption.value shouldBe TextExpression(
      IfElse(
        TopLevelRef(BooleanExprId("noCompanyName")),
        IfElse(Equals(LangCtx, Constant("en")), Constant("Are you"), Constant("Wyt ti")),
        Add(IfElse(Equals(LangCtx, Constant("en")), Constant("Is "), Constant("Os ")), FormCtx("companyName"))
      )
    )

  }

  it should "parse hmrcTaxPeriodCtx as DateCtx" in {
    val res = ValueParser.validate("${returnTaxPeriod.periodTo}")
    res.toOption.value shouldBe TextExpression(
      DateCtx(
        HmrcTaxPeriodCtx(FormCtx(FormComponentId("returnTaxPeriod")), HmrcTaxPeriodInfo.PeriodTo)
      )
    )
  }

  it should "parse selectedCountry.column.InEU" in {
    val res = ValueParser.validate("${selectedCountry.column.InEU}")
    res.toOption.value should be(TextExpression(CsvCountryCheck(FormComponentId("selectedCountry"), "InEU")))
  }

  it should "parse applicantNationality.column.InEU.count('1')" in {
    val res = ValueParser.validate("${applicantNationality.column.InEU.count('1')}")
    res.toOption.value should be(
      TextExpression(CsvCountryCountCheck(FormComponentId("applicantNationality"), "InEU", "1"))
    )
  }

  it should "parse applicantNationality.column.Name.count('United Kingdom')" in {
    val res = ValueParser.validate("${applicantNationality.column.Name.count('United Kingdom')}")
    res.toOption.value should be(
      TextExpression(CsvCountryCountCheck(FormComponentId("applicantNationality"), "Name", "United Kingdom"))
    )
  }

  it should "parse selectedCountry.country.column.InEU" in {
    val res = ValueParser.validate("${selectedCountry.country.column.InEU}")
    res.toOption.value should be(TextExpression(CsvOverseasCountryCheck(FormComponentId("selectedCountry"), "InEU")))
  }

  it should "parse company registration number - name" in {
    val res = ValueParser.validate("${dataRetrieve.companyRegistrationNumber.name}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("companyRegistrationNumber"), DataRetrieve.Attribute("name"))
      )
    )
  }

  it should "parse company registration number - status" in {
    val res = ValueParser.validate("${dataRetrieve.companyRegistrationNumber.status}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("companyRegistrationNumber"), DataRetrieve.Attribute("status"))
      )
    )
  }

  it should "parse company registration number - registeredAddress" in {
    val res = ValueParser.validate("${dataRetrieve.companyRegistrationNumber.registeredAddress}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("companyRegistrationNumber"), DataRetrieve.Attribute("registeredAddress"))
      )
    )
  }

  it should "parse nino insights - riskScore" in {
    val res = ValueParser.validate("${dataRetrieve.ninoInsights.riskScore}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("ninoInsights"), DataRetrieve.Attribute("riskScore"))
      )
    )
  }

  it should "parse nino insights - reason" in {
    val res = ValueParser.validate("${dataRetrieve.ninoInsights.reason}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("ninoInsights"), DataRetrieve.Attribute("reason"))
      )
    )
  }

  it should "parse bank account insights - riskScore" in {
    val res = ValueParser.validate("${dataRetrieve.bankCheck.riskScore}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("bankCheck"), DataRetrieve.Attribute("riskScore"))
      )
    )
  }

  it should "parse bank account insights - reason" in {
    val res = ValueParser.validate("${dataRetrieve.bankCheck.reason}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("bankCheck"), DataRetrieve.Attribute("reason"))
      )
    )
  }

  it should "parse personal bank account existence - accountExists" in {
    val res = ValueParser.validate("${dataRetrieve.personalBankAccountExistence.accountExists}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("personalBankAccountExistence"), DataRetrieve.Attribute("accountExists"))
      )
    )
  }

  it should "parse personal bank account existence - nameMatches" in {
    val res = ValueParser.validate("${dataRetrieve.personalBankAccountExistence.nameMatches}")
    res.toOption.value should be(
      TextExpression(
        DataRetrieveCtx(DataRetrieveId("personalBankAccountExistence"), DataRetrieve.Attribute("nameMatches"))
      )
    )
  }

  it should "parse substring function" in {
    val res = ValueParser.validate("${substring(fieldId, 3, 4)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.SubString(3, 4)))
    )
  }

  it should "parse removeSpaces function" in {
    val res = ValueParser.validate("${removeSpaces(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.RemoveSpaces))
    )
  }

  it should "parse capitalize function" in {
    val res = ValueParser.validate("${capitalize(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.Capitalize))
    )
  }

  it should "parse capitalizeAll function" in {
    val res = ValueParser.validate("${capitalizeAll(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.CapitalizeAll))
    )
  }

  it should "parse uppercase function" in {
    val res = ValueParser.validate("${uppercase(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.UpperCase))
    )
  }

  it should "parse lowercase function" in {
    val res = ValueParser.validate("${lowercase(fieldId)}")
    res.toOption.value should be(
      TextExpression(StringOps(FormCtx(FormComponentId("fieldId")), StringFnc.LowerCase))
    )
  }

  it should "parse lowercase, uppercase, and capitalize functions" in {
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

  it should "parse concat function" in {
    val res = ValueParser.validate("${concat(fieldId, ' test')}")
    res.toOption.value should be(
      TextExpression(Concat(List(FormCtx(FormComponentId("fieldId")), Constant(" test"))))
    )
  }

  it should "parse auth.itmpAddress.country function" in {
    val res = ValueParser.validate("${auth.itmpAddress.country}")
    res.toOption.value should be(
      TextExpression(CountryOfItmpAddress)
    )
  }

  it should "parse link.download.nipClaimScheduleTemplate.xlsx function" in {
    val res = ValueParser.validate("${link.download.nipClaimScheduleTemplate.xlsx}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Download("nipClaimScheduleTemplate.xlsx")))
    )
  }

  it should "parse link.download.nipClaimScheduleTemplate.ods function" in {
    val res = ValueParser.validate("${link.download.nipClaimScheduleTemplate.ods}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Download("nipClaimScheduleTemplate.ods")))
    )
  }

  it should "parse link.image.imagename.png function" in {
    val res = ValueParser.validate("${link.image.imagename.png}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.Image("imagename.png")))
    )
  }

  it should "parse choicesRevealedField function" in {
    val res = ValueParser.validate("${choicesRevealedField(fieldId)}")
    res.toOption.value should be(
      TextExpression(ChoicesRevealedField(FormComponentId("fieldId")))
    )
  }

  it should "parse link.'https://www.gov.uk' function" in {
    val res = ValueParser.validate("${link.'https://www.gov.uk'}")
    res.toOption.value should be(
      TextExpression(LinkCtx(InternalLink.UrlLink("https://www.gov.uk")))
    )
  }

  it should "parse choice function" in {
    val res = ValueParser.validate("${choice(fieldId)}")
    res.toOption.value should be(
      TextExpression(ChoiceLabel(FormComponentId("fieldId")))
    )
  }

  it should "parse the hideZeroDecimals function" in {
    val res = ValueParser.validate("${hideZeroDecimals(age)}")

    res.toOption.value should be(TextExpression(HideZeroDecimals(FormCtx("age"))))
  }

  it should "parse the hideZeroDecimals function with other functions" in {
    val res = ValueParser.validate("${hideZeroDecimals(age + 2)}")

    res.toOption.value should be(TextExpression(HideZeroDecimals(Add(FormCtx("age"), Constant("2")))))
  }

  it should "parse substring function with user field enrolments" in {
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

}
