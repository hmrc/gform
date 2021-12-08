/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.libs.json.{ JsError, JsPath, Json, JsonValidationError }
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, ValidateBankDetails }

class DataRetrieveSpec extends FlatSpec with Matchers {

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
      FormCtx(FormComponentId("companyName"))
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
}
