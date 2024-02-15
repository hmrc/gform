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

package uk.gov.hmrc.gform.formtemplate

import io.circe.Json
import io.circe.literal.JsonStringContext
import munit.FunSuite
import play.api.libs.json.{ JsArray, JsValue }
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

class JsonSchemeValidatorSpec extends FunSuite {

  private def constructTestOneSectionJsonTemplate(
    properties: Json
  ): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [
          {
            "title": "Page",
            "fields": [
               $properties
            ]
          }
        ]
      }"""

  private def constructTestTwoSectionJsonTemplate(properties1: Json, properties2: Json) =
    json"""
    {
      "_id": "json-id",
      "formName": "Json",
      "version": 1,
      "description": "",
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            $properties1
          ]
        },
        {
          "title": "Page 2",
          "fields": [
            $properties2
          ]
        }
      ]
    }"""

  private def constructTestTwoFieldJsonTemplate(properties1: Json, properties2: Json) =
    json"""
    {
      "_id": "json-id",
      "formName": "Json",
      "version": 1,
      "description": "",
      "sections": [
        {
          "title": "Page 1",
          "fields": [
            $properties1,
            $properties2
          ]
        }
      ]
    }"""

  private def getNumberOfErrors(errorsAsList: List[JsValue]): Int = {
    // Get the number of custom conditional validation error messages
    val numberOfConditionalValidationErrors = errorsAsList
      .map(_.toString().contains("can only be used with"))
      .count(_ == true)

    // Get the number of property type error messages
    val numberOfTypeErrors = errorsAsList
      .map(_.toString().contains("expected type"))
      .count(_ == true)

    numberOfConditionalValidationErrors + numberOfTypeErrors
  }

  private def getConditionalValidationErrors(errorsAsList: List[JsValue], numberOfErrors: Int): List[JsValue] =
    // Only need last errors because conditional validation messages will appear at the bottom of the stack trace
    errorsAsList.slice(errorsAsList.length - numberOfErrors, errorsAsList.length)

  private def runInvalidJsonTest(result: Either[SchemaValidationException, Unit], expectedResult: List[String]): Unit =
    result match {
      case Right(value) => fail("No error was returned from schema validation:\n" + value)

      case Left(errors) =>
        val errorsAsList = errors.errors.as[JsArray].value.toList

        val numberOfErrors = getNumberOfErrors(errorsAsList)
        assertEquals(numberOfErrors, expectedResult.length)

        val conditionalValidationErrors = getConditionalValidationErrors(errorsAsList, numberOfErrors)
        assertEquals(conditionalValidationErrors, expectedResult.map(toJson(_)))
    }

  test(
    "validateJson rejects the form gracefully when the property infoType is used and the type property is not [info]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "infoType": "noformat"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List("Error at ID <testId>: Property infoType can only be used with type: [info]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property infoText is used and the type property is not [info]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "infoText": "Test infoText"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List("Error at ID <testId>: Property infoText can only be used with type: [info]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property choices is used and the type property is not [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "choices": [
              "Test choice 1",
              "Test choice 2"
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("Error at ID <testId>: Property choices can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property multivalue is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "multivalue": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("Error at ID <testId>: Property multivalue can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property hints is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "hints": [
              "Test hint 1",
              "Test hint 2"
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("Error at ID <testId>: Property hints can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property optionHelpText is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "optionHelpText": [
              {
                "en": "Test English optionHelpText",
                "cy": "Test Welsh optionHelpText"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property optionHelpText can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dividerPosition is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "dividerPosition": 1
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dividerPosition can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dividerText is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "dividerText": {
              "en": "Test English dividerText",
              "cy": "Test Welsh dividerText"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dividerText can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property noneChoice is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "noneChoice": 1
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property noneChoice can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property noneChoiceError is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "noneChoiceError": {
              "en": "Test English noneChoiceError",
              "cy": "Test Welsh noneChoiceError"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property noneChoiceError can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the type property is not [text]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "multiline": "true",
            "displayCharCount": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the multiline property is not [true]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "multiline": "false",
            "displayCharCount": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the multiline property is not present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "displayCharCount": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the type property is not [text] and the multiline property is not [true]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "multiline": "false",
            "displayCharCount": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the type property is not [text]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "multiline": "true",
            "dataThreshold": 75
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the multiline property is not [true]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "multiline": "false",
            "dataThreshold": 75
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the multiline property is not present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "dataThreshold": 75
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the type property is not [text] and the multiline property is not [true]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "multiline": "false",
            "dataThreshold": 75
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in one section and the type property is not [info]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "infoText": "Test infoText",
            "infoType": "noformat"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property infoText can only be used with type: [info]",
      "Error at ID <testId>: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when all properties dependent on type property being [choice, revealingChoice] are used in one section with invalid type"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "choices": [
              "Test choice 1",
              "Test choice 2"
            ],
            "multivalue": "true",
            "hints": [
              "Test hint 1",
              "Test hint 2"
            ],
            "optionHelpText": [
              {
                "en": "Test English optionHelpText",
                "cy": "Test Welsh optionHelpText"
              }
            ],
            "dividerPosition": 1,
            "dividerText": {
              "en": "Test English dividerText",
              "cy": "Test Welsh dividerText"
            },
            "noneChoice": 1,
            "noneChoiceError": {
              "en": "Test English noneChoiceError",
              "cy": "Test Welsh noneChoiceError"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property hints can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property dividerPosition can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property noneChoice can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property multivalue can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property optionHelpText can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property dividerText can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property choices can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property noneChoiceError can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when properties with different dependencies all have errors"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "infoType": "noformat",
            "choices": ["Test choice 1", "Test choice 2"],
            "dataThreshold": 1
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]",
      "Error at ID <testId>: Property infoType can only be used with type: [info]",
      "Error at ID <testId>: Property choices can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property format is used and the type property is not [text, choice, date, group]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "format": "yesno"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property format can only be used with type: [text, choice, date, group]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property cityMandatory is used and the type property is not [address, overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "cityMandatory": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property cityMandatory can only be used with type: [address, overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property countyDisplayed is used and the type property is not [address]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "countyDisplayed": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property countyDisplayed can only be used with type: [address]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property international is used and the type property is not [address]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "date",
            "international": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property international can only be used with type: [address]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property countryDisplayed is used and the type property is not [overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "group",
            "countryDisplayed": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property countryDisplayed can only be used with type: [overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property countryLookup is used and the type property is not [overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "group",
            "countryLookup": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property countryLookup can only be used with type: [overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property line2Mandatory is used and the type property is not [overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "group",
            "line2Mandatory": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property line2Mandatory can only be used with type: [overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property line3Mandatory is used and the type property is not [overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "group",
            "line3Mandatory": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property line3Mandatory can only be used with type: [overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property postcodeMandatory is used and the type property is not [overseasAddress]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "date",
            "postcodeMandatory": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property postcodeMandatory can only be used with type: [overseasAddress]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property confirmAddressLabel is used and the type property is not [postcodeLookup]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "confirmAddressLabel": "Confirm the test business address"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property confirmAddressLabel can only be used with type: [postcodeLookup]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property chooseAddressLabel is used and the type property is not [postcodeLookup]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "chooseAddressLabel": "Choose the test business address"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property chooseAddressLabel can only be used with type: [postcodeLookup]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in different fields when the type property is not [info]"
  ) {
    val testProperties1 =
      json"""
          {
            "id": "TestID1",
            "label": "Test label 1",
            "type": "text",
            "infoText": "Test infoText"
          }
          """

    val testProperties2 =
      json"""
          {
            "id": "TestID2",
            "label": "Test label 2",
            "type": "choice",
            "infoType": "noformat"
          }
          """

    val jsonTemplate = constructTestTwoFieldJsonTemplate(testProperties1, testProperties2)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <TestID1>: Property infoText can only be used with type: [info]",
      "Error at ID <TestID2>: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in different fields when the type property is not [info] in one of the fields"
  ) {
    val testProperties1 =
      json"""
          {
            "id": "TestID1",
            "label": "Test label 1",
            "type": "info",
            "infoText": "Test infoText"
          }
          """

    val testProperties2 =
      json"""
          {
            "id": "TestID2",
            "label": "Test label 2",
            "type": "choice",
            "infoType": "noformat"
          }
          """

    val jsonTemplate = constructTestTwoFieldJsonTemplate(testProperties1, testProperties2)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <TestID2>: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in different sections when the type property is not [info]"
  ) {
    val testProperties1 =
      json"""
          {
            "id": "TestID1",
            "label": "Test label 1",
            "type": "text",
            "infoText": "Test infoText"
          }
          """

    val testProperties2 =
      json"""
          {
            "id": "TestID2",
            "label": "Test label 2",
            "type": "choice",
            "infoType": "noformat"
          }
          """

    val jsonTemplate = constructTestTwoSectionJsonTemplate(testProperties1, testProperties2)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <TestID1>: Property infoText can only be used with type: [info]",
      "Error at ID <TestID2>: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when displayCharCount and dataThreshold are used in different sections when the type property is not [text] in one section and the multiline property is not [true] in the other"
  ) {
    val testProperties1 =
      json"""
            {
              "id": "TestID1",
              "label": "Test label 1",
              "type": "text",
              "displayCharCount": "false"
            }
            """

    val testProperties2 =
      json"""
            {
              "id": "TestID2",
              "label": "Test label 2",
              "type": "info",
              "multiline": "true",
              "dataThreshold": 1
            }
            """

    val jsonTemplate = constructTestTwoSectionJsonTemplate(testProperties1, testProperties2)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <TestID1>: Property displayCharCount can only be used with type: [text], multiline: [true]",
      "Error at ID <TestID2>: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the properties dependent on type [info] are present but the type property is missing"
  ) {
    val testProperties =
      json"""
            {
              "id": "testId",
              "label": "test label",
              "infoText": "Test infoText",
              "infoType": "noformat"
            }
          """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property infoText can only be used with type: [info]",
      "Error at ID <testId>: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the properties dependent on type [choice, revealingChoice] are present but the type property is missing"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "choices": [
              "Test choice 1",
              "Test choice 2"
            ],
            "multivalue": "true",
            "hints": [
              "Test hint 1",
              "Test hint 2"
            ],
            "optionHelpText": [
              {
                "en": "Test English optionHelpText",
                "cy": "Test Welsh optionHelpText"
              }
            ],
            "dividerPosition": 1,
            "dividerText": {
              "en": "Test English dividerText",
              "cy": "Test Welsh dividerText"
            },
            "noneChoice": 1,
            "noneChoiceError": {
              "en": "Test English noneChoiceError",
              "cy": "Test Welsh noneChoiceError"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property hints can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property dividerPosition can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property noneChoice can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property multivalue can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property optionHelpText can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property dividerText can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property choices can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property noneChoiceError can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the properties dependent on type [text] and multiline [true] are present but the type and multiline properties are missing"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "dataThreshold": 75,
            "displayCharCount": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]",
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [info] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
            {
              "id": "testId",
              "label": "test label",
              "type": "info",
              "infoText": "Test infoText",
              "infoType": "noformat"
            }
          """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [text] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "text"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [choice] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              "Test choice 1",
              "Test choice 2"
            ],
            "multivalue": "true",
            "hints": [
              "Test hint 1",
              "Test hint 2"
            ],
            "optionHelpText": [
              {
                "en": "Test English optionHelpText",
                "cy": "Test Welsh optionHelpText"
              }
            ],
            "dividerPosition": 1,
            "dividerText": {
              "en": "Test English dividerText",
              "cy": "Test Welsh dividerText"
            },
            "noneChoice": 1,
            "noneChoiceError": {
              "en": "Test English noneChoiceError",
              "cy": "Test Welsh noneChoiceError"
            },
            "format": "yesno"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [revealingChoice] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "revealingChoice",
            "choices": [
              "Test choice 1",
              "Test choice 2"
            ],
            "multivalue": "true",
            "hints": [
              "Test hint 1",
              "Test hint 2"
            ],
            "optionHelpText": [
              {
                "en": "Test English optionHelpText",
                "cy": "Test Welsh optionHelpText"
              }
            ],
            "dividerPosition": 1,
            "dividerText": {
              "en": "Test English dividerText",
              "cy": "Test Welsh dividerText"
            },
            "noneChoice": 1,
            "noneChoiceError": {
              "en": "Test English noneChoiceError",
              "cy": "Test Welsh noneChoiceError"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [date] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "date",
            "format": "before today"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [group] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "group",
            "format": "horizontal"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [text], the multiline property is [true], and the properties that are dependent on these are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "multiline": "true",
            "displayCharCount": "false",
            "dataThreshold": 75
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [address] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "address",
            "cityMandatory": "true",
            "countyDisplayed": "false",
            "international": "false"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [overseasAddress] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "overseasAddress",
            "cityMandatory": "false",
            "countryDisplayed": "false",
            "countryLookup": "true",
            "line2Mandatory": "true",
            "line3Mandatory": "false",
            "postcodeMandatory": "true"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [postcodeLookup] and the properties that are dependent on this are present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "postcodeLookup",
            "confirmAddressLabel": "Confirm the test business address",
            "chooseAddressLabel": "Choose the test business address"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson rejects the form and includes relevant errors messages when the types of properties are incorrect"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": [
              "type is not an array"
            ],
            "multiline": 1,
            "displayCharCount": {
              "en": "displayCharCount is not an object"
            },
            "dataThreshold": "dataThreshold is not a string"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0/type: expected type: String, found: JSONArray",
      "#/sections/0/fields/0/dataThreshold: expected type: Integer, found: String",
      "#/sections/0/fields/0/multiline: expected type: String, found: Integer",
      "#/sections/0/fields/0/displayCharCount: expected type: String, found: JSONObject"
    )

    runInvalidJsonTest(result, expectedResult)
  }
}
