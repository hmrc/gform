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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{ JsError, JsPath, Json, JsonValidationError }
import uk.gov.hmrc.gform.sharedmodel.{ Attr, AttributeInstruction, ConstructAttribute, DataRetrieve, DataRetrieveId, Fetch }

class DataRetrieveSpec extends AnyFlatSpec with Matchers {

  "Json.parse" should "parse json as ValidateBankDetails" in {
    Json
      .parse("""
               |{
               |  "type": "validateBankDetails",
               |  "id": "bankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCodeExpr}",
               |    "accountNumber": "${accountNumberExpr}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("validateBankDetails"),
      DataRetrieveId("bankDetails"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("isValid"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
        )
      ),
      Map(),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("sortCode", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("sortCodeExpr"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("accountNumber", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("accountNumberExpr"))
        )
      ),
      None
    )
  }

  it should "parse json as BusinessBankAccountExistence" in {
    Json
      .parse("""
               |{
               |  "type": "businessBankAccountExistence",
               |  "id": "businessBankAccount",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}",
               |    "companyName": "${companyName}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("businessBankAccountExistence"),
      DataRetrieveId("businessBankAccount"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("accountNumberIsWellFormatted"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("accountExists"),
            ConstructAttribute.AsIs(Fetch(List("accountExists")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nameMatches"),
            ConstructAttribute.AsIs(Fetch(List("nameMatches")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          )
        )
      ),
      Map(),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("sortCode", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("sortCode"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("accountNumber", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("accountNumber"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("companyName", List("business"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("companyName"))
        )
      ),
      None
    )
  }

  it should "return error when 'type' is not string" in {
    Json
      .parse("""
               |{
               |  "type": {},
               |  "id": "bankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .validateOpt[DataRetrieve] shouldBe JsError(
      s"Type of value is invalid for attribute 'type' [error=${List((JsPath(), Seq(JsonValidationError(Seq("error.expected.jsstring")))))}]"
    )
  }

  it should "return error when 'id' is missing" in {
    Json
      .parse("""
               |{
               |  "type": "validateBankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .validateOpt[DataRetrieve] shouldBe JsError("'id' attribute missing")
  }

  it should "return error when 'type' is missing" in {
    Json
      .parse("""
               |{
               |  "id": "bankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .validateOpt[DataRetrieve] shouldBe JsError("'type' attribute missing")
  }

  it should "parse json as CompanyRegistrationNumber" in {
    Json
      .parse("""
               |{
               |  "type": "companyRegistrationNumber",
               |  "id": "companyRegistration",
               |  "parameters": {
               |    "companyNumber": "${companyNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("companyRegistrationNumber"),
      DataRetrieveId("companyRegistration"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("companyName"),
            ConstructAttribute.AsIs(Fetch(List("company_name")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("companyStatus"),
            ConstructAttribute.AsIs(Fetch(List("company_status")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("registeredOfficeAddress"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("registered_office_address", "address_line_1")),
                Fetch(List("registered_office_address", "address_line_2")),
                Fetch(List("registered_office_address", "postal_code")),
                Fetch(List("registered_office_address", "locality")),
                Fetch(List("registered_office_address", "region"))
              )
            )
          )
        )
      ),
      Map(),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("companyNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("companyNumber"))
        )
      ),
      None
    )
  }

  it should "parse json as NinoInsights" in {
    Json
      .parse("""
               |{
               |  "type": "ninoInsights",
               |  "id": "ninoCheck",
               |  "parameters": {
               |    "nino": "${nino}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("ninoInsights"),
      DataRetrieveId("ninoCheck"),
      Attr.FromObject(
        List(
          AttributeInstruction(DataRetrieve.Attribute("riskScore"), ConstructAttribute.AsIs(Fetch(List("riskScore")))),
          AttributeInstruction(DataRetrieve.Attribute("reason"), ConstructAttribute.AsIs(Fetch(List("reason"))))
        )
      ),
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Integer),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("nino", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("nino"))
        )
      ),
      None
    )
  }

  it should "parse json as BankAccountInsights" in {
    Json
      .parse("""
               |{
               |  "type": "bankAccountInsights",
               |  "id": "bankCheck",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("bankAccountInsights"),
      DataRetrieveId("bankCheck"),
      Attr.FromObject(
        List(
          AttributeInstruction(DataRetrieve.Attribute("riskScore"), ConstructAttribute.AsIs(Fetch(List("riskScore")))),
          AttributeInstruction(DataRetrieve.Attribute("reason"), ConstructAttribute.AsIs(Fetch(List("reason"))))
        )
      ),
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Integer),
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("sortCode", List(), DataRetrieve.ParamType.String),
            FormCtx(FormComponentId("sortCode"))
          ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("accountNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("accountNumber"))
        )
      ),
      None
    )
  }

  it should "parse json as BankAccountInsights with if" in {
    Json
      .parse("""
               |{
               |  "type": "bankAccountInsights",
               |  "id": "bankCheck",
               |  "if": "${true}",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("bankAccountInsights"),
      DataRetrieveId("bankCheck"),
      Attr.FromObject(
        List(
          AttributeInstruction(DataRetrieve.Attribute("riskScore"), ConstructAttribute.AsIs(Fetch(List("riskScore")))),
          AttributeInstruction(DataRetrieve.Attribute("reason"), ConstructAttribute.AsIs(Fetch(List("reason"))))
        )
      ),
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Integer),
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("sortCode", List(), DataRetrieve.ParamType.String),
            FormCtx(FormComponentId("sortCode"))
          ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("accountNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("accountNumber"))
        )
      ),
      Some(IncludeIf(IsTrue))
    )
  }

  it should "parse json as PersonalBankAccountExistence" in {
    Json
      .parse("""
               |{
               |  "type": "personalBankAccountExistence",
               |  "id": "personalBankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accNumber}",
               |    "firstName": "${firstName}",
               |    "lastName": "${lastName}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("personalBankAccountExistence"),
      DataRetrieveId("personalBankDetails"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("accountNumberIsWellFormatted"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("accountExists"),
            ConstructAttribute.AsIs(Fetch(List("accountExists")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nameMatches"),
            ConstructAttribute.AsIs(Fetch(List("nameMatches")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("accountName"),
            ConstructAttribute.AsIs(Fetch(List("accountName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
        )
      ),
      Map(),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("sortCode", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("sortCode"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("accountNumber", List("account"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("accNumber"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("firstName", List("subject"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("firstName"))
        ),
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("lastName", List("subject"), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("lastName"))
        )
      ),
      None
    )
  }

  it should "return error when name, firstName and lastName are missing for PersonalBankAccountExistence" in {
    Json
      .parse("""
               |{
               |  "type": "personalBankAccountExistence",
               |  "id": "personalBankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accNumber}"
               |  }
               |}
               |""".stripMargin)
      .validateOpt[DataRetrieve] shouldBe JsError("'firstName' attribute missing")
  }
}
