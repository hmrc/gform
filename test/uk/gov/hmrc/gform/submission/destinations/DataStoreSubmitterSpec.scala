/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.destinations

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.sdes.datastore.DataStoreWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId

import scala.concurrent.ExecutionContext.Implicits.global

class DataStoreSubmitterSpec extends AnyWordSpecLike with Matchers with MockFactory {
  private val objectStoreAlgebra = mock[ObjectStoreAlgebra[FOpt]]
  private val dataStoreWorkItemAlgebra = mock[DataStoreWorkItemAlgebra[FOpt]]

  private val dataStoreSubmitter = new DataStoreSubmitter(objectStoreAlgebra, dataStoreWorkItemAlgebra)

  private val destinationId = DestinationId("dataStoreIlluminate")
  private val payloadDiscriminator = "handlebar"

  private def convertIncorrectJsonToError(
    incorrectJson: String,
    specificError: String,
    errorLine: Int,
    errorColumn: Int
  ): String = {
    val errorStart: String =
      s"""Data store destination 'dataStoreIlluminate' error, failed to parse handlebar payload into a json:
         |$specificError
         | at [Source: (String)"""".stripMargin

    val errorEnd: String = s""""; line: $errorLine, column: $errorColumn], full json:\n$incorrectJson"""

    errorStart + incorrectJson + errorEnd
  }

  ".convertToJson" should {
    "replace the incorrect JSON's strings with their lengths in the error thrown" when {
      "given a string of a simple JSON object with an incorrectly formatted number with a decimal point at the end" when {
        "a string field has no special characters" in {
          val incorrectJson =
            """{
              |  "firstName": "John",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "4",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has no special characters and there is no space after the colon" in {
          val incorrectJson =
            """{
              |  "firstName":"John",
              |  "number":1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName":"4",
              |  "number":1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 14)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \" at the start" in {
          val incorrectJson =
            """{
              |  "firstName": ""John",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "5",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError = "Unexpected character ('J' (code 74)): was expecting comma to separate Object entries"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 2, 19)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\" at the start" in {
          val incorrectJson =
            """{
              |  "firstName": "\"John",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "6",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\"\\\" at the start" in {
          val incorrectJson =
            """{
              |  "firstName": "\"\"John",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "8",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \" at the end" in {
          val incorrectJson =
            """{
              |  "firstName": "John"",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "5",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError = "Unexpected character ('\"' (code 34)): was expecting comma to separate Object entries"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 2, 23)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\" at the end" in {
          val incorrectJson =
            """{
              |  "firstName": "John\"",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "6",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\"\\\" at the end" in {
          val incorrectJson =
            """{
              |  "firstName": "John\"\"",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "8",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \" in the middle" in {
          val incorrectJson =
            """{
              |  "firstName": "Jo"hn",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "5",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError = "Unexpected character ('h' (code 104)): was expecting comma to separate Object entries"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 2, 21)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\" in the middle" in {
          val incorrectJson =
            """{
              |  "firstName": "Jo\"hn",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "6",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "a string field has a \\\"\\\" in the middle" in {
          val incorrectJson =
            """{
              |  "firstName": "Jo\"\"hn",
              |  "number": 1.
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "8",
              |  "number": 1.
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are string fields in the first and last positions with no special characters" in {
          val incorrectJson =
            """{
              |  "firstName": "John",
              |  "number": 1.,
              |  "lastName": "Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "4",
              |  "number": 1.,
              |  "lastName": "7"
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are string fields in the first and last positions where the first has special characters" in {
          val incorrectJson =
            """{
              |  "firstName": "John\"",
              |  "number": 1.,
              |  "lastName": "Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "6",
              |  "number": 1.,
              |  "lastName": "7"
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are string fields in the first and last positions where the last has special characters" in {
          val incorrectJson =
            """{
              |  "firstName": "John",
              |  "number": 1.,
              |  "lastName": "\"Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "4",
              |  "number": 1.,
              |  "lastName": "9"
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are string fields in first and last positions where both have special characters" in {
          val incorrectJson =
            """{
              |  "firstName": "\"John",
              |  "number": 1.,
              |  "lastName": "Johnson\"\""
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "6",
              |  "number": 1.,
              |  "lastName": "11"
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }
      }

      "given a string of a simple JSON object with an incorrectly formatted string" when {
        "there are multiple string fields with the first incorrectly formatted by containing \"" in {
          val incorrectJson =
            """{
              |  "firstName": "John"",
              |  "middleName": "Gregory",
              |  "lastName": "Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "5",
              |  "middleName": "7",
              |  "lastName": "7"
              |}
              |""".stripMargin

          val specificError = """Unexpected character ('"' (code 34)): was expecting comma to separate Object entries"""

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 2, 23)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are multiple string fields with the last incorrectly formatted by containing \"" in {
          val incorrectJson =
            """{
              |  "firstName": "John",
              |  "middleName": "Gregory",
              |  "lastName": ""Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "4",
              |  "middleName": "7",
              |  "lastName": "8"
              |}
              |""".stripMargin

          val specificError = "Unexpected character ('J' (code 74)): was expecting comma to separate Object entries"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 4, 18)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "there are multiple string fields with the middle incorrectly formatted by containing \"" in {
          val incorrectJson =
            """{
              |  "firstName": "John",
              |  "middleName": "Gregory",
              |  "lastName": ""Johnson"
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "firstName": "4",
              |  "middleName": "7",
              |  "lastName": "8"
              |}
              |""".stripMargin

          val specificError = "Unexpected character ('J' (code 74)): was expecting comma to separate Object entries"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 4, 18)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }
      }

      "given a string of a JSON with nesting" when {
        "the first JSON object has an incorrectly formatted number with a decimal point at the end" when {
          "all string fields in all objects are formatted correctly" in {
            val incorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "John",
                |    "age": 50.,
                |    "middleName": "Gregory",
                |    "lastName": "Johnson"
                |  },
                |  "businessDetails": {
                |    "name": "Test Business",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val convertedIncorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "4",
                |    "age": 50.,
                |    "middleName": "7",
                |    "lastName": "7"
                |  },
                |  "businessDetails": {
                |    "name": "13",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val specificError =
              "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

            val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 4, 15)

            val exception = intercept[Exception](
              dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
            )
            exception shouldBe a[Exception]
            exception.getMessage shouldBe expectedErrorMessage
          }

          "a string field in one of the objects is incorrectly formatted" in {
            val incorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "John",
                |    "age": 50.,
                |    "middleName": "Gregory",
                |    "lastName": ""Johnson"
                |  },
                |  "businessDetails": {
                |    "name": "Test Business",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val convertedIncorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "4",
                |    "age": 50.,
                |    "middleName": "7",
                |    "lastName": "8"
                |  },
                |  "businessDetails": {
                |    "name": "13",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val specificError =
              "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

            val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 4, 15)

            val exception = intercept[Exception](
              dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
            )
            exception shouldBe a[Exception]
            exception.getMessage shouldBe expectedErrorMessage
          }

          "all string fields in all objects are incorrectly formatted" in {
            val incorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "J"ohn",
                |    "age": 50.,
                |    "middleName": "Gregory"",
                |    "lastName": ""Johnson"
                |  },
                |  "businessDetails": {
                |    "name": "Test Bus"iness",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val convertedIncorrectJson =
              """{
                |  "personDetails": {
                |    "firstName": "5",
                |    "age": 50.,
                |    "middleName": "8",
                |    "lastName": "8"
                |  },
                |  "businessDetails": {
                |    "name": "14",
                |    "yearIncorporated": 2020
                |  }
                |}
                |""".stripMargin

            val specificError = "Unexpected character ('o' (code 111)): was expecting comma to separate Object entries"

            val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 3, 22)

            val exception = intercept[Exception](
              dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
            )
            exception shouldBe a[Exception]
            exception.getMessage shouldBe expectedErrorMessage
          }
        }

        "the second JSON object has an incorrectly formatted number with a decimal point at the end" in {
          val incorrectJson =
            """{
              |  "personDetails": {
              |    "firstName": "John",
              |    "age": 50,
              |    "middleName": "Gregory",
              |    "lastName": "Johnson"
              |  },
              |  "businessDetails": {
              |    "name": "Test Business",
              |    "yearIncorporated": 2020.
              |  }
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "personDetails": {
              |    "firstName": "4",
              |    "age": 50,
              |    "middleName": "7",
              |    "lastName": "7"
              |  },
              |  "businessDetails": {
              |    "name": "13",
              |    "yearIncorporated": 2020.
              |  }
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character ((CTRL-CHAR, code 10)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 10, 30)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }

        "both JSON objects have an incorrectly formatted number with a decimal point at the end" in {
          val incorrectJson =
            """{
              |  "personDetails": {
              |    "firstName": "John",
              |    "age": 50.,
              |    "middleName": "Gregory",
              |    "lastName": "Johnson"
              |  },
              |  "businessDetails": {
              |    "name": "Test Business",
              |    "yearIncorporated": 2020.
              |  }
              |}
              |""".stripMargin

          val convertedIncorrectJson =
            """{
              |  "personDetails": {
              |    "firstName": "4",
              |    "age": 50.,
              |    "middleName": "7",
              |    "lastName": "7"
              |  },
              |  "businessDetails": {
              |    "name": "13",
              |    "yearIncorporated": 2020.
              |  }
              |}
              |""".stripMargin

          val specificError =
            "Unexpected character (',' (code 44)) in numeric value: Decimal point not followed by a digit"

          val expectedErrorMessage = convertIncorrectJsonToError(convertedIncorrectJson, specificError, 4, 15)

          val exception = intercept[Exception](
            dataStoreSubmitter.convertToJson(incorrectJson, destinationId, payloadDiscriminator)
          )
          exception shouldBe a[Exception]
          exception.getMessage shouldBe expectedErrorMessage
        }
      }
    }
  }
}
