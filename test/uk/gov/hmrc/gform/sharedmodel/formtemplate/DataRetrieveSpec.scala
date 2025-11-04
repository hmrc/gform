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
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthInfo.PayeNino

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
      None,
      None,
      None
    )
  }

  it should "parse json as BusinessBankAccountExistence" in {
    Json
      .parse("""
               |{
               |  "type": "businessBankAccountExistence",
               |  "id": "businessBankAccount",
               |  "maxFailedAttempts": 3,
               |  "failureCountResetMinutes": 1440,
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
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("accountName"),
            ConstructAttribute.AsIs(Fetch(List("accountName")))
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
      None,
      Some(3),
      Some(1440)
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

  it should "parse json as CompanyHouseProfile" in {
    Json
      .parse("""
               |{
               |  "type": "companyHouseProfile",
               |  "id": "companyRegistration",
               |  "parameters": {
               |    "companyNumber": "${companyNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("companyHouseProfile"),
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
            DataRetrieve.Attribute("address_line_1"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "address_line_1")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_2"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "address_line_2")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("po_box"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "po_box")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("locality"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "locality")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("region"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "region")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("postal_code"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "postal_code")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("country"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "country")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("registeredOfficeAddress"),
            ConstructAttribute.Combine(
              List(
                (DataRetrieve.Attribute("address_line_1"), Fetch(List("registered_office_address", "address_line_1"))),
                (DataRetrieve.Attribute("address_line_2"), Fetch(List("registered_office_address", "address_line_2"))),
                (DataRetrieve.Attribute("po_box"), Fetch(List("registered_office_address", "po_box"))),
                (DataRetrieve.Attribute("locality"), Fetch(List("registered_office_address", "locality"))),
                (DataRetrieve.Attribute("region"), Fetch(List("registered_office_address", "region"))),
                (DataRetrieve.Attribute("postal_code"), Fetch(List("registered_office_address", "postal_code"))),
                (DataRetrieve.Attribute("country"), Fetch(List("registered_office_address", "country")))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("dateOfCreation"),
            ConstructAttribute.AsIs(Fetch(List("date_of_creation")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode1"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 0)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode2"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 1)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode3"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 2)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode4"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 3)
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("dateOfCreation") -> DataRetrieve.AttrType.Date
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("companyNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("companyNumber"))
        )
      ),
      None,
      None,
      None
    )
  }

  it should "parse json as CompanyHouseActiveOfficers" in {
    Json
      .parse("""
               |{
               |  "type": "companyHouseActiveOfficers",
               |  "id": "directors",
               |  "parameters": {
               |    "companyNumber": "${companyNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("companyHouseActiveOfficers"),
      DataRetrieveId("directors"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("activeDirectors"),
            ConstructAttribute.AsIs(Fetch(List("active_directors")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeSecretaries"),
            ConstructAttribute.AsIs(Fetch(List("active_secretaries")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeLlpMembers"),
            ConstructAttribute.AsIs(Fetch(List("active_llp_members")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("activeDirectors")   -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeSecretaries") -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeLlpMembers")  -> DataRetrieve.AttrType.Number
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("companyNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("companyNumber"))
        )
      ),
      None,
      None,
      None
    )
  }

  it should "parse json as CompanyHouseInsolvency" in {
    Json
      .parse("""
               |{
               |  "type": "companyHouseInsolvency",
               |  "id": "insolvency",
               |  "parameters": {
               |    "companyNumber": "${companyNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("companyHouseInsolvency"),
      DataRetrieveId("insolvency"),
      Attr.FromArray(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("caseType"),
            ConstructAttribute.AsIs(Fetch(List("caseType")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("caseNumber"),
            ConstructAttribute.AsIs(Fetch(List("caseNumber")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("name"),
            ConstructAttribute.AsIs(Fetch(List("name")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("role"),
            ConstructAttribute.AsIs(Fetch(List("role")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_1"),
            ConstructAttribute.AsIs(Fetch(List("address", "address_line_1")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_2"),
            ConstructAttribute.AsIs(Fetch(List("address", "address_line_2")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("locality"),
            ConstructAttribute.AsIs(Fetch(List("address", "locality")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("region"),
            ConstructAttribute.AsIs(Fetch(List("address", "region")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("postal_code"),
            ConstructAttribute.AsIs(Fetch(List("address", "postal_code")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("country"),
            ConstructAttribute.AsIs(Fetch(List("address", "country")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("primaryAddress"),
            ConstructAttribute.Combine(
              List(
                DataRetrieve.Attribute("address_line_1") -> Fetch(List("address", "address_line_1")),
                DataRetrieve.Attribute("address_line_2") -> Fetch(List("address", "address_line_2")),
                DataRetrieve.Attribute("locality")       -> Fetch(List("address", "locality")),
                DataRetrieve.Attribute("region")         -> Fetch(List("address", "region")),
                DataRetrieve.Attribute("postal_code")    -> Fetch(List("address", "postal_code")),
                DataRetrieve.Attribute("country")        -> Fetch(List("address", "country"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("appointedOn"),
            ConstructAttribute.AsIs(Fetch(List("appointedOn")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("caseType")       -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("caseNumber")     -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("name")           -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("role")           -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("address_line_1") -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("address_line_1") -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("locality")       -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("region")         -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("postal_code")    -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("country")        -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("appointedOn")    -> DataRetrieve.AttrType.Date
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("companyNumber", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("companyNumber"))
        )
      ),
      None,
      None,
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
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Number),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("nino", List(), DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("nino"))
        )
      ),
      None,
      None,
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
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Number),
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
      None,
      None,
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
      Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Number),
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
      Some(IncludeIf(IsTrue)),
      None,
      None
    )
  }

  it should "parse json as PersonalBankAccountExistence" in {
    Json
      .parse("""
               |{
               |  "type": "personalBankAccountExistence",
               |  "id": "personalBankDetails",
               |  "maxFailedAttempts": 3,
               |  "failureCountResetMinutes": 1440,
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
      None,
      Some(3),
      Some(1440)
    )
  }

  it should "parse json as hmrcRosmRegistrationCheck with if" in {
    Json
      .parse("""
               |{
               |  "type": "hmrcRosmRegistrationCheck",
               |  "id": "hmrcRosmCheck",
               |  "parameters": {
               |    "regime": "${'APD'}",
               |    "utr": "${utr}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("hmrcRosmRegistrationCheck"),
      DataRetrieveId("hmrcRosmCheck"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("postalCode"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "address", "InternationalAddress", "postalCode")),
                Fetch(List("det", "address", "UkAddress", "postalCode"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("safeId"),
            ConstructAttribute.AsIs(Fetch(List("det", "safeId")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("organisationName"),
            ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "organisationName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("organisationType"),
            ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "organisationType")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("isAGroup"),
            ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "isAGroup")))
          )
        )
      ),
      Map.empty,
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("regime", List(), DataRetrieve.ParamType.String),
            Constant("APD")
          ),
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("utr", List(), DataRetrieve.ParamType.String),
            FormCtx(FormComponentId("utr"))
          )
      ),
      None,
      None,
      None
    )
  }

  it should "parse json as agentDetails with if" in {
    Json
      .parse("""
               |{
               |  "type": "agentDetails",
               |  "id": "agencyInfo",
               |  "parameters": {
               |      "agentReferenceNumber": "${agentReferenceNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("agentDetails"),
      DataRetrieveId("agencyInfo"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("agencyName"),
            ConstructAttribute.AsIs(Fetch(List("det", "agencyDetails", "agencyName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_1"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine1")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine1"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_2"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine2")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine2"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("locality"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine3")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine3"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("region"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine4")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine4"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("postal_code"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "postalCode")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "postalCode"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("country"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "countryCode")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "countryCode"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("agencyAddress"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine1")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine2")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine3")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine4")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "postalCode")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine1")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine2")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine3")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine4")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "postalCode")),
                Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "countryCode"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("agencyEmail"),
            ConstructAttribute.AsIs(Fetch(List("det", "agencyDetails", "agencyEmail")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("agencyPhone"),
            ConstructAttribute.AsIs(Fetch(List("det", "contactDetails", "phoneNumber")))
          )
        )
      ),
      Map.empty,
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("agentReferenceNumber", List(), DataRetrieve.ParamType.String),
            FormCtx(FormComponentId("agentReferenceNumber"))
          )
      ),
      None,
      None,
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

  it should "parse json as hmrcTaxRates with" in {
    Json
      .parse("""
               |{
               |  "type": "hmrcTaxRates",
               |  "id": "apdRate",
               |  "parameters": {
               |    "regime": "${'APD'}",
               |    "code": "${'BANDB-RDCD'}",
               |    "date": "${userDate}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("hmrcTaxRates"),
      DataRetrieveId("apdRate"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("regime"),
            ConstructAttribute.AsIs(Fetch(List("regime")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("code"),
            ConstructAttribute.AsIs(Fetch(List("code")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("rate"),
            ConstructAttribute.AsIs(Fetch(List("rate")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("startDate"),
            ConstructAttribute.AsIs(Fetch(List("startDate")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("endDate"),
            ConstructAttribute.AsIs(Fetch(List("endDate")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("regime")    -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("code")      -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("rate")      -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("startDate") -> DataRetrieve.AttrType.Date,
        DataRetrieve.Attribute("endDate")   -> DataRetrieve.AttrType.Date
      ),
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("regime", List(), DataRetrieve.ParamType.String),
            Constant("APD")
          ),
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("code", List(), DataRetrieve.ParamType.String),
            Constant("BANDB-RDCD")
          ),
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("date", List(), DataRetrieve.ParamType.Date),
            FormCtx(FormComponentId("userDate"))
          )
      ),
      None,
      None,
      None
    )
  }

  it should "parse json as niRefundClaim with" in {
    Json
      .parse("""
               |{
               |  "type": "niRefundClaim",
               |  "id": "refundDetails",
               |  "parameters": {
               |    "nino": "${auth.payenino}",
               |    "claimReference": "${claimReferenceNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("niRefundClaim"),
      DataRetrieveId("refundDetails"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("refundType"),
            ConstructAttribute.AsIs(Fetch(List("RefundDetails", "refundType")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("taxYear"),
            ConstructAttribute.AsIs(Fetch(List("RefundDetails", "taxYear")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("class2ContributionWeeks"),
            ConstructAttribute.AsIs(Fetch(List("RefundDetails", "class2ContributionWeeks")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("class3ContributionWeeks"),
            ConstructAttribute.AsIs(Fetch(List("RefundDetails", "class3ContributionWeeks")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("weeksOfCredits"),
            ConstructAttribute.AsIs(Fetch(List("RefundDetails", "weeksOfCredits")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("refundType")              -> DataRetrieve.AttrType.String,
        DataRetrieve.Attribute("taxYear")                 -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("class2ContributionWeeks") -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("class3ContributionWeeks") -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("weeksOfCredits")          -> DataRetrieve.AttrType.Number
      ),
      List(
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("nino", List(), DataRetrieve.ParamType.String),
            AuthCtx(PayeNino)
          ),
        DataRetrieve
          .ParamExpr(
            DataRetrieve.Parameter("claimReference", List(), DataRetrieve.ParamType.String),
            FormCtx(FormComponentId("claimReferenceNumber"))
          )
      ),
      None,
      None,
      None
    )
  }

  it should "return error when code is missing from hmrcTaxRates with" in {
    Json
      .parse("""
               |{
               |  "type": "hmrcTaxRates",
               |  "id": "apdRate",
               |  "parameters": {
               |    "regime": "${'APD'}",
               |    "date": "${userDate}"
               |  }
               |}
               |""".stripMargin)
      .validateOpt[DataRetrieve] shouldBe JsError("'code' attribute missing")
  }

  it should "parse json as delegatedAgentAuthVat" in {
    Json
      .parse("""
               |{
               |  "type": "delegatedAgentAuthVat",
               |  "id": "agentAuthorisedForVRN",
               |  "parameters": {
               |    "vatRegistrationNumber": "${vrn}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("delegatedAgentAuthVat"),
      DataRetrieveId("agentAuthorisedForVRN"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("authorised"),
            ConstructAttribute.AsIs(Fetch(List("authorised")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("authorised") -> DataRetrieve.AttrType.String
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("vatRegistrationNumber", List.empty[String], DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("vrn"))
        )
      ),
      None,
      None,
      None
    )
  }

  it should "parse json as delegatedAgentAuthPaye" in {
    Json
      .parse("""
               |{
               |  "type": "delegatedAgentAuthPaye",
               |  "id": "agentAuthorisedForPAYE",
               |  "parameters": {
               |    "payeReference": "${payeRef}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe DataRetrieve(
      DataRetrieve.Type("delegatedAgentAuthPaye"),
      DataRetrieveId("agentAuthorisedForPAYE"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("authorised"),
            ConstructAttribute.AsIs(Fetch(List("authorised")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("authorised") -> DataRetrieve.AttrType.String
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("payeReference", List.empty[String], DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("payeRef"))
        )
      ),
      None,
      None,
      None
    )
  }

}
