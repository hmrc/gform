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
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, ValidateBankDetails }

class DataRetrieveSpec extends AnyFlatSpec with Matchers {

  "Json.parse" should "parse json as ValidateBankDetails" in {
    Json
      .parse("""
               |{
               |  "type": "validateBankDetails",
               |  "id": "bankDetails",
               |  "parameters": {
               |    "sortCode": "${sortCode}",
               |    "accountNumber": "${accountNumber}"
               |  }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe ValidateBankDetails(
      DataRetrieveId("bankDetails"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accountNumber"))
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
      .as[DataRetrieve] shouldBe BusinessBankAccountExistence(
      DataRetrieveId("businessBankAccount"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accountNumber")),
      FormCtx(FormComponentId("companyName")),
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

  it should "fallback to default reads" in {
    Json
      .parse("""
               |{
               |   "ValidateBankDetails":{
               |      "id":"bankDetails",
               |      "sortCode":{
               |         "FormCtx":{
               |            "formComponentId":"sortCode"
               |         }
               |      },
               |      "accountNumber":{
               |         "FormCtx":{
               |            "formComponentId":"accountNumber"
               |         }
               |      }
               |   }
               |}
               |""".stripMargin)
      .as[DataRetrieve] shouldBe ValidateBankDetails(
      DataRetrieveId("bankDetails"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accountNumber"))
    )
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
      .as[DataRetrieve] shouldBe DataRetrieve.CompanyRegistrationNumber(
      DataRetrieveId("companyRegistration"),
      FormCtx(FormComponentId("companyNumber")),
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
      .as[DataRetrieve] shouldBe DataRetrieve.NinoInsights(
      DataRetrieveId("ninoCheck"),
      FormCtx(FormComponentId("nino")),
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
      .as[DataRetrieve] shouldBe DataRetrieve.BankAccountInsights(
      DataRetrieveId("bankCheck"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accountNumber")),
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
      .as[DataRetrieve] shouldBe DataRetrieve.BankAccountInsights(
      DataRetrieveId("bankCheck"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accountNumber")),
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
      .as[DataRetrieve] shouldBe DataRetrieve.PersonalBankAccountExistence(
      DataRetrieveId("personalBankDetails"),
      FormCtx(FormComponentId("sortCode")),
      FormCtx(FormComponentId("accNumber")),
      FormCtx(FormComponentId("firstName")),
      FormCtx(FormComponentId("lastName")),
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
