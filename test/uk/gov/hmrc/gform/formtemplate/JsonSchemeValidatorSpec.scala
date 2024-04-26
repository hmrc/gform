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
import play.api.libs.json.JsArray
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

class JsonSchemeValidatorSpec extends FunSuite {

  private def constructTestOneSectionJsonTemplate(properties: Json): Json =
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

  private def constructTestSummarySectionJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [],
        "summarySection": $properties
      }"""

  private def constructTestTaskListJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [
          {
            "tasks": [
              $properties
            ]
          }
        ]
      }"""

  private def constructTestTaskListSummarySectionJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [
          {
            "tasks": [
              {
                "sections": [],
                "summarySection": $properties
              }
            ]
          }
        ]
      }"""

  private def constructTestDeclarationSectionJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [],
        "declarationSection": $properties
      }"""

  private def constructTestTaskListDeclarationSectionJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [
          {
            "tasks": [
              {
                "sections": [],
                "declarationSection": $properties
              }
            ]
          }
        ]
      }"""

  private def constructTestAcknowledgementSectionJsonTemplate(properties: Json): Json =
    json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [],
        "acknowledgementSection": $properties
      }"""

  private def runInvalidJsonTest(result: Either[SchemaValidationException, Unit], expectedResult: List[String]): Unit =
    result match {
      case Right(value) => fail("No error was returned from schema validation:\n" + value)

      case Left(errors) =>
        val errorsAsList = errors.errors.as[JsArray].value.toList

        val conditionalValidationErrors = errorsAsList.filter(_.toString().contains("Error at"))
        assertEquals(conditionalValidationErrors, expectedResult.map(toJson(_)))
    }

  def validateJson(json: Json): Either[SchemaValidationException, Unit] = {
    val inputStream = getClass.getClassLoader.getResourceAsStream("formTemplateSchema.json")
    val schema = scala.io.Source.fromInputStream(inputStream).mkString

    JsonSchemaValidator.checkSchema(json.toString, schema, JsonSchemaErrorParser.parseErrorMessages)
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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property infoType can only be used with type: [info]",
      "Error at ID <testId>: Property infoText can only be used with type: [info]"
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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dividerPosition can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property hints can only be used with type: [choice, revealingChoice]",
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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property infoType can only be used with type: [info]",
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]",
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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property chooseAddressLabel can only be used with type: [postcodeLookup]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property prefix is used and the format property is [sterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "sterling",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property prefix can only be used with type: [text], format not: [sterling, positiveSterling, positiveWholeSterling]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property prefix is used and the format property is [positiveSterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveSterling",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property prefix can only be used with type: [text], format not: [sterling, positiveSterling, positiveWholeSterling]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property prefix is used and the format property is [positiveWholeSterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveWholeSterling",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property prefix can only be used with type: [text], format not: [sterling, positiveSterling, positiveWholeSterling]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property prefix is used and the type property is not [text]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "format": "number",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property prefix can only be used with type: [text], format not: [sterling, positiveSterling, positiveWholeSterling]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property prefix is used with the type and format properties not present"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property prefix can only be used with type: [text], format not: [sterling, positiveSterling, positiveWholeSterling]"
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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property infoType can only be used with type: [info]",
      "Error at ID <testId>: Property infoText can only be used with type: [info]"
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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dividerPosition can only be used with type: [choice, revealingChoice]",
      "Error at ID <testId>: Property hints can only be used with type: [choice, revealingChoice]",
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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property dataThreshold can only be used with type: [text], multiline: [true]",
      "Error at ID <testId>: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the types of properties are incorrect"
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

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property type expected type [String], found [JSONArray]",
      "Error at ID <testId>: Property dataThreshold expected type [Integer], found [String]",
      "Error at ID <testId>: Property multiline expected type [String], found [Integer]",
      "Error at ID <testId>: Property displayCharCount expected type [String], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a property outside of a field (no field ID) is incorrect"
  ) {
    val jsonTemplate =
      json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": true,
        "sections": []
      }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/description>: Property description expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a property outside of a field (no field ID) and a property inside a field are incorrect"
  ) {
    val jsonTemplate =
      json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": {
          "cy": "Cy test description"
        },
        "sections": [
          {
            "title": "Page 1",
            "fields": [
              {
                "id": "testId",
                "label": {
                  "value": "test label"
                }
              }
            ]
          }
        ]
      }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/description>: Property description expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required",
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a StringOrEnCyObject property contains one incorrect property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {
              "value": "test label"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a StringOrEnCyObject property contains more than one incorrect property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {
              "value": "test label",
              "anotherValue": "another test label"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value, anotherValue] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a StringOrEnCyObject property only contains the Cy property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {
              "cy": "Cy test label"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a StringOrEnCyObject property contains no properties"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {}
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when multiple StringOrEnCyObject properties contain incorrect properties"
  ) {
    val testProperties1 =
      json"""
          {
            "id": "TestID1",
            "label": {
              "xyz": "Test label 1"
            }
          }
          """

    val testProperties2 =
      json"""
          {
            "id": "TestID2",
            "shortName": {
              "cy": "Test label 2"
            }
          }
          """

    val jsonTemplate = constructTestTwoFieldJsonTemplate(testProperties1, testProperties2)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <TestID1>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [xyz] are not permitted",
      "Error at ID <TestID2>: Property shortName expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a StringOrEnCyObject property takes a value of an incorrect type"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": 1
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property label expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property takes a value of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": "this string should not be allowed"
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property choices expected type [Array], found [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property contains both strings and objects"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            "this is a string",
            {
              "en": "this is an object"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property contains invalid types"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            false,
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property contains one valid type and one invalid type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            "this is a string",
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has an object with an invalid key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "val": "this should not be allowed"
            },
            {
              "en": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Invalid key(s) [val] are not permitted",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has an object missing a required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "cy": "test cy 1"
            },
            {
              "cy": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Missing key(s) [en] are required",
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has an object with an invalid key and a missing required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "xyz": "this should not be allowed"
            },
            {
              "en": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Missing key(s) [en] are required. Invalid key(s) [xyz] are not permitted",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has one object with an invalid key and another object missing a required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "xyz": "this should not be allowed"
            },
            {
              "cy": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Invalid key(s) [xyz] are not permitted",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has one object with a primitive-typed property of invalid type and one valid object"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "cy": true
            },
            {
              "en": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property cy expected type [String], found [Boolean]",
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has one object with a stringOrEnCyObject-typed property of invalid type and one valid object"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "hint": {
                "xyz": "this should not be allowed"
              }
            },
            {
              "en": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property hint expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [xyz] are not permitted",
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has one invalid object and one object with an invalid property"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "hint": {
                "xyz": "this should not be allowed"
              }
            },
            {
              "abc": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]. Missing key(s) [en] are required. Invalid key(s) [abc] are not permitted",
      "Error at ID <testId: choices/0>: Property hint expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [xyz] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has multiple objects with all properties invalid"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": 1,
              "cy": false,
              "dynamic": {},
              "hint": [],
              "value": 1,
              "includeIf": null
            },
            {
              "en": {},
              "cy": [],
              "dynamic": true,
              "hint": 1,
              "value": [],
              "includeIf": 4
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/0>: Property includeIf expected type [String], found [Null]",
      "Error at ID <testId: choices/0>: Property cy expected type [String], found [Boolean]",
      "Error at ID <testId: choices/0>: Property en expected type [String], found [Integer]",
      "Error at ID <testId: choices/0>: Property dynamic expected type [String], found [JSONObject]",
      "Error at ID <testId: choices/0>: Property value expected type [String], found [Integer]",
      "Error at ID <testId: choices/1>: Property includeIf expected type [String], found [Integer]",
      "Error at ID <testId: choices/1>: Property cy expected type [String], found [JSONArray]",
      "Error at ID <testId: choices/1>: Property hint expected type String or JSONObject with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: choices/1>: Property en expected type [String], found [JSONObject]",
      "Error at ID <testId: choices/1>: Property dynamic expected type [String], found [Boolean]",
      "Error at ID <testId: choices/1>: Property value expected type [String], found [JSONArray]"
    )
    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a choices property has one object with an invalid property type and one index of the array is an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "choice",
          "choices": [
            {
              "en": "test en 1",
              "cy": true
            },
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: choices/0>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/1>: Property choices expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]",
      "Error at ID <testId: choices/0>: Property cy expected type [String], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property is used when the type property is not present"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "header": [
            "header 1",
            "header 2"
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property header can only be used with type: [table]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property is used when the type property is not [table]"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "text",
          "header": [
            "header 1",
            "header 2"
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property header can only be used with type: [table]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property takes a value of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": "This should be an array"
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId>: Property header expected type [Array], found [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property contains both strings and objects"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            "this is a string",
            {
              "en": "this is an object"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property contains invalid types"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            false,
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property contains one valid type and one invalid type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            "this is a string",
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has an object with an invalid key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "en": "test en 1",
              "val": "this should not be allowed"
            },
            {
              "en": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}. Invalid key(s) [val] are not permitted",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has an object missing a required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "en": "test en 1",
              "cy": "test cy 1"
            },
            {
              "cy": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required",
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has an object with an invalid key and a missing required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "xyz": "this should not be allowed"
            },
            {
              "en": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [xyz] are not permitted",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has one object with an invalid key and another object missing a required key"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "en": "test en 1",
              "xyz": "this should not be allowed"
            },
            {
              "cy": "test cy 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}. Invalid key(s) [xyz] are not permitted",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has one object with a property of invalid type and one valid object"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "en": "test en 1",
              "cy": true
            },
            {
              "en": "test en 2"
            }
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property cy expected type [String], found [Boolean]",
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a header property has one object with an invalid property type and one index of the array is an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "id": "testId",
          "label": "test label",
          "type": "table",
          "header": [
            {
              "en": "test en 1",
              "cy": true
            },
            1
          ]
        }
      """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <testId: header/0>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/1>: Property header expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}",
      "Error at ID <testId: header/0>: Property cy expected type [String], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection does not have any of the required properties [title, header, footer]"
  ) {
    val testProperties =
      json"""
        {
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection>: summarySection requires properties [title, header, footer] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection only has one of the required properties [title, header, footer]"
  ) {
    val testProperties =
      json"""
        {
          "title": "yes"
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection>: summarySection requires properties [title, header, footer] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has one invalid property key"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "type": "text"
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection>: summarySection has invalid key(s) [type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has multiple invalid property keys"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "type": "text",
          "format": "text"
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection>: summarySection has invalid key(s) [format, type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the title property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": 1,
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          }
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the header property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": true,
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          }
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/header>: Property header expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the footer property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": false
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/footer>: Property footer expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the displayWidth property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": 1
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/displayWidth>: Property displayWidth expected type [String], found [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the displayWidth property of a not permitted value"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": "ml"
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/displayWidth>: Property displayWidth expected value [m, l, xl]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the continueLabel property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "continueLabel": false
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the pdf property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "pdf": "pdf should not be a string"
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/pdf>: Property pdf expected type [Object], found [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the note property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "note": true
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/note>: Property note expected type [String], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the fields property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": {}
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/fields>: Property fields expected type [Array], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the fields property containing an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": [
            "incorrect"
          ]
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/fields/0>: Property fields expected type Array of [Object], found Array of [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main CYA summarySection has the fields property containing an invalid property key"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": [
            {
              "summarySection": {}
            }
          ]
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/summarySection/fields/0>: fields has invalid key(s) [summarySection]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection does not have any of the required properties [title, header, footer]"
  ) {
    val testProperties =
      json"""
        {
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection>: summarySection requires properties [title, header, footer] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection only has one of the required properties [title, header, footer]"
  ) {
    val testProperties =
      json"""
        {
          "title": "yes"
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection>: summarySection requires properties [title, header, footer] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has one invalid property key"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "type": "text"
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection>: summarySection has invalid key(s) [type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has multiple invalid property keys"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "type": "text",
          "format": "text"
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection>: summarySection has invalid key(s) [format, type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the title property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": 1,
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          }
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the header property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": true,
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          }
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/header>: Property header expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the footer property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": false
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/footer>: Property footer expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the displayWidth property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": 1
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/displayWidth>: Property displayWidth expected type [String], found [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the displayWidth property of a not permitted value"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": "ml"
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/displayWidth>: Property displayWidth expected value [m, l, xl]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the continueLabel property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "continueLabel": false
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the includeIf property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "includeIf": {
            "value": "includeIf should not be an object"
          }
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/includeIf>: Property includeIf expected type [String], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the fields property of an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": {}
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/fields>: Property fields expected type [Array], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the fields property containing an incorrect type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": [
            {
              "excludeFromPdf": false
            }
          ]
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/fields/0>: The excludeFromPdf only be defined on main summarySection"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list summarySection has the fields property containing an invalid property key"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "fields": [
            {
              "summarySection": {}
            }
          ]
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/summarySection/fields/0>: fields has invalid key(s) [summarySection]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when emailCodeParameters has a value that is not a StringOrEnCy Object"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "emailCodeParameters": [
            {
              "value": 1,
              "emailTemplateVariable": "emailSubjectCode"
            }
          ]
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/emailCodeParameters/0/value>: Property value expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig does not include the allowedReferrerUrls property"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            },
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig>: referrerConfig requires properties [allowedReferrerUrls, exitMessage] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig does not include the exitMessage property"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig>: referrerConfig requires properties [allowedReferrerUrls, exitMessage] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig includes a property that is not one of [allowedReferrerUrls, exitMessage, title]"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            },
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            },
            "format": "text"
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig>: referrerConfig has invalid key(s) [format]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig includes allowedReferrerUrls that isn't an Array"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": true,
            "exitMessage": "exit message",
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig/allowedReferrerUrls>: Property allowedReferrerUrls expected type [Array], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig includes allowedReferrerUrls that is an Array of not Strings"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              1
            ],
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            },
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig/allowedReferrerUrls/0>: Property allowedReferrerUrls expected type Array of [String], found Array of [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig includes exitMessage that is not a StringOrEnCy Object"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "exitMessage": {
              "notAllowed": 4
            },
            "title": {
              "en": "English title",
              "cy": "Welsh title"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig/exitMessage>: Property exitMessage expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [notAllowed] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when referrerConfig includes title that is not a StringOrEnCy Object"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            },
            "title": false
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/referrerConfig/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection does not contain the property [title]"
  ) {
    val testProperties =
      json"""
        {
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection>: declarationSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [title] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": false,
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection does not contain the property [fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1"
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection>: declarationSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [fields] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": true
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/fields>: Property fields expected type [Array], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [fields] which is an array of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [
            1
          ]
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/fields/0>: Property fields expected type Array of [Object], found Array of [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [fields] which is an array of objects that are invalid"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [
            {
              "id": "test-declarationSection-id",
              "type": 1
            }
          ]
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <test-declarationSection-id>: Property type expected type [String], found [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [shortName] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": 1,
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/shortName>: Property shortName expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [continueLabel] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": true,
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains the property [includeIf] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": {},
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/includeIf>: Property includeIf expected type [String], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains a property that isn't any of [title, shortName, continueLabel, includeIf, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [],
          "type": "text"
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection>: declarationSection has invalid key(s) [type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a main declarationSection contains StringOrEnCyObjects that are all invalid"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title",
            "value": "value not allowed"
          },
          "shortName": {
            "cy": "Welsh short name"
          },
          "continueLabel": 1,
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/declarationSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Invalid key(s) [value] are not permitted",
      "Error at <#/declarationSection/shortName>: Property shortName expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required",
      "Error at <#/declarationSection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection does not contain the property [title]"
  ) {
    val testProperties =
      json"""
        {
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection>: declarationSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [title] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": false,
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection does not contain the property [fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1"
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection>: declarationSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [fields] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": true
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/fields>: Property fields expected type [Array], found [Boolean]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [fields] which is an array of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [
            {
              "excludeFromPdf": false
            }
          ]
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/fields/0>: The excludeFromPdf only be defined on main summarySection"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [fields] which is an array of objects that are invalid"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [
            {
              "id": "test-declarationSection-id",
              "type": 1
            }
          ]
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <test-declarationSection-id>: Property type expected type [String], found [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [shortName] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": 1,
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/shortName>: Property shortName expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [continueLabel] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": true,
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains the property [includeIf] of an invalid type"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": {},
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/includeIf>: Property includeIf expected type [String], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains a property that isn't any of [title, shortName, continueLabel, includeIf, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": [],
          "type": "text"
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection>: declarationSection has invalid key(s) [type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task list declarationSection contains StringOrEnCyObjects that are all invalid"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title",
            "value": "value not allowed"
          },
          "shortName": {
            "cy": "Welsh short name"
          },
          "continueLabel": 1,
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/sections/0/tasks/0/declarationSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Invalid key(s) [value] are not permitted",
      "Error at <#/sections/0/tasks/0/declarationSection/shortName>: Property shortName expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required",
      "Error at <#/sections/0/tasks/0/declarationSection/continueLabel>: Property continueLabel expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection does not contain the required property [title]"
  ) {
    val testProperties =
      json"""
      {
        "panelTitle": {
          "en": "English panel title",
          "cy": "Welsh panel title"
        },
        "showReference": true,
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          }
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection>: acknowledgementSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection does not contain the required property [fields]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "en": "English panel title",
          "cy": "Welsh panel title"
        },
        "showReference": true,
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          }
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true
        }
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection>: acknowledgementSection requires properties [title, fields] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains an invalid property"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "en": "English panel title",
          "cy": "Welsh panel title"
        },
        "showReference": true,
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          }
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true
        },
        "fields": [],
        "type": "text"
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection>: acknowledgementSection has invalid key(s) [type]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [title] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": 1,
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [title] as an object without the key [en]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "cy": "Welsh title"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [title] as an object with a key other than [en, cy]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title",
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [title] as an object without the key [en] with a key other than [en, cy]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "cy": "Welsh title",
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/title>: Property title expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [panelTitle] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": false,
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/panelTitle>: Property panelTitle expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [panelTitle] as an object without the key [en]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "cy": "Welsh panel title"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/panelTitle>: Property panelTitle expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [panelTitle] as an object with a key other than [en, cy]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "en": "English panel title",
          "cy": "Welsh panel title",
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/panelTitle>: Property panelTitle expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [panelTitle] as an object without the key [en] with a key other than [en, cy]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "cy": "Welsh panel title",
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/panelTitle>: Property panelTitle expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value] are not permitted"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [showReference] with an invalid type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "showReference": [],
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/showReference>: Property showReference expected type [Boolean], found [JSONArray]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [instructionPdf] with an invalid type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "instructionPdf": "This is not allowed",
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/instructionPdf>: Property instructionPdf expected type [Object], found [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [instructionPdf] that includes an invalid property"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/instructionPdf>: instructionPdf has invalid key(s) [value]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [instructionPdf] that doesn't include the property [header]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "instructionPdf": {
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          }
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/instructionPdf>: instructionPdf requires properties [header] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [instructionPdf] that has the property [footer] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": 1
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/instructionPdf/footer>: Property footer expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [pdf] that includes an invalid property"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true,
          "value": "This is not allowed"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/pdf>: pdf has invalid key(s) [value]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [pdf] that doesn't include the property [header]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "pdf": {
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/pdf>: pdf requires properties [header] to be present"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [pdf] that has the property [tabularFormat] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": {},
          "includeSignatureBox": true
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/pdf/tabularFormat>: Property tabularFormat expected type [Boolean], found [JSONObject]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [pdf] that has the property [includeSignatureBox] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": []
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/pdf/includeSignatureBox>: Property includeSignatureBox expected type [Boolean], found [JSONArray]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [fields] of an incorrect type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "fields": "This is not allowed"
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/fields>: Property fields expected type [Array], found [String]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [fields] which is an array of an invalid type"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "fields": [
          1
        ]
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at <#/acknowledgementSection/fields/0>: Property fields expected type Array of [Object], found Array of [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when an acknowledgementSection contains the property [fields] which is an array of objects that are invalid"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "fields": [
          {
            "id": "test-acknowledgementSection-id",
            "type": 1
          }
        ]
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = List(
      "Error at ID <test-acknowledgementSection-id>: Property type expected type [String], found [Integer]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property errorExample is an object missing the en key and including an invalid key"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "date",
            "errorExample": {
              "value": "This is not allowed",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult =
      List(
        "Error at ID <testId>: Property errorExample expected type String or JSONObject with structure {en: String} or {en: String, cy: String}. Missing key(s) [en] are required. Invalid key(s) [value] are not permitted"
      )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property errorExample is used with the format property not present and the type property not [taxPeriodDate, calendarDate, date, address]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "info",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult =
      List(
        "Error at ID <testId>: Property errorExample can only be used with type: [taxPeriodDate, calendarDate, date, address] OR type: [text], format: [number, positiveNumber, positiveWholeNumber, sterling, positiveSterling, positiveWholeSterling]"
      )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property errorExample is used with the type property set to [text] and no format property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult =
      List(
        "Error at ID <testId>: Property errorExample can only be used with type: [taxPeriodDate, calendarDate, date, address] OR type: [text], format: [number, positiveNumber, positiveWholeNumber, sterling, positiveSterling, positiveWholeSterling]"
      )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property errorExample is used with the type property set to [text] and the format property not [number, positiveNumber, positiveWholeNumber, sterling, positiveSterling, positiveWholeSterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "text",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult =
      List(
        "Error at ID <testId>: Property errorExample can only be used with type: [taxPeriodDate, calendarDate, date, address] OR type: [text], format: [number, positiveNumber, positiveWholeNumber, sterling, positiveSterling, positiveWholeSterling]"
      )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when a task contains a caption that is not a stringOrEnCy object"
  ) {
    val testProperties =
      json"""
          {
            "title": "Test title",
            "caption": true,
            "sections": []
          }
        """

    val jsonTemplate = constructTestTaskListJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult =
      List(
        "Error at <#/sections/0/tasks/0/caption>: Property caption expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

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

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the prefix property is present with format not [sterling, wholeSterling, positiveWholeSterling] and type [text]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "number",
            "prefix": "£"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a StringOrEnCyObject property is a String"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label"
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a StringOrEnCyObject property is an object with just the En property defined"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {
              "en": "test label"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a StringOrEnCyObject property is an object with both the En and Cy properties defined"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": {
              "en": "En test label",
              "cy": "Cy test label"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a choices property is an Array of Strings"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              "choice 1",
              "choice 2",
              "choice 3"
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a choices property is an Array of Objects with just the En property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              {
                "en": "en choice 1"
              },
              {
                "en": "en choice 2"
              },
              {
                "en": "en choice 3"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a choices property is an Array of Objects with all valid properties"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              {
                "en": "en choice 1",
                "cy": "cy choice 1",
                "dynamic": "dynamic.one",
                "hint": {
                  "en": "en hint 1",
                  "cy": "cy hint 1"
                },
                "value": "NONE",
                "includeIf": "1=1"
              },
              {
                "en": "en choice 2",
                "cy": "cy choice 2",
                "dynamic": "dynamic.two",
                "hint": {
                  "en": "en hint 2",
                  "cy": "cy hint 2"
                },
                "value": "NONE",
                "includeIf": "1=2"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a choices property is an Array of one Object with all valid properties and one Object with just the En property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              {
                "en": "en choice 1",
                "cy": "cy choice 1",
                "dynamic": "dynamic.one",
                "hint": {
                  "en": "en hint 1",
                  "cy": "cy hint 1"
                },
                "value": "NONE",
                "includeIf": "1=1"
              },
              {
                "en": "en choice 2"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a header property is an Array of Strings"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "table",
            "header": [
              "header 1",
              "header 2",
              "header 3"
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a header property is an Array of Objects with just the En property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "table",
            "header": [
              {
                "en": "en header 1"
              },
              {
                "en": "en header 2"
              },
              {
                "en": "en header 3"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a header property is an Array of Objects with En and Cy properties"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "table",
            "header": [
              {
                "en": "en header 1",
                "cy": "cy header 1"
              },
              {
                "en": "en header 2",
                "cy": "cy header 2"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a header property is an Array of one Object with the En and Cy properties, and one Object with just the En property"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "table",
            "header": [
              {
                "en": "en header 1",
                "cy": "cy header 1"
              },
              {
                "en": "en header 2"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a main CYA summarySection has all required and optional properties with correct types and values"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": "xl",
          "continueLabel": "correct continueLabel string",
          "pdf": {},
          "note": "note",
          "fields": []
        }
      """

    val jsonTemplate = constructTestSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task list summarySection has all required and optional properties with correct types and values"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "en title",
            "cy": "cy title"
          },
          "header": {
            "en": "en header",
            "cy": "cy header"
          },
          "footer": {
            "en": "en footer",
            "cy": "cy footer"
          },
          "displayWidth": "xl",
          "continueLabel": "correct continueLabel string",
          "includeIf": "include if",
          "fields": []
        }
      """

    val jsonTemplate = constructTestTaskListSummarySectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when emailCodeParameters has a value that is a String"
  ) {
    val jsonTemplate =
      json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [],
        "emailCodeParameters": [
          {
            "value": "a valid value",
            "emailTemplateVariable": "emailSubjectCode"
          }
        ]
      }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when emailCodeParameters has a value that is an Object with En and Cy properties"
  ) {
    val jsonTemplate =
      json"""
      {
        "_id": "json-id",
        "formName": "Json",
        "version": 1,
        "description": "",
        "sections": [],
        "emailCodeParameters": [
          {
            "value": {
              "en": "English value",
              "cy": "Welsh value"
            },
            "emailTemplateVariable": "emailSubjectCode"
          }
        ]
      }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when referrerConfig includes valid [allowedReferrerUrls, exitMessage]"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            }
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when referrerConfig includes valid [allowedReferrerUrls, exitMessage, title]"
  ) {
    val jsonTemplate =
      json"""
        {
          "_id": "json-id",
          "formName": "Json",
          "version": 1,
          "description": "",
          "sections": [],
          "referrerConfig": {
            "allowedReferrerUrls": [
              "tax.service.gov.uk"
            ],
            "exitMessage": {
              "en": "English exit message",
              "cy": "Welsh exit message"
            },
            "title": "Title"
          }
        }"""

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a main declarationSection includes valid [title, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a main declarationSection includes valid [title, shortName, continueLabel, includeIf, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task list declarationSection includes valid [title, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task list declarationSection includes valid [title, shortName, continueLabel, includeIf, fields]"
  ) {
    val testProperties =
      json"""
        {
          "title": {
            "en": "English title",
            "cy": "Welsh title"
          },
          "shortName": {
            "en": "English short name",
            "cy": "Welsh short name"
          },
          "continueLabel": {
            "en": "English continue label",
            "cy": "Welsh continue label"
          },
          "includeIf": "1=1",
          "fields": []
        }"""

    val jsonTemplate = constructTestTaskListDeclarationSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when an acknowledgementSection includes valid [title, fields]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when an acknowledgementSection includes valid [title, panelTitle, showReference, instructionPdf, pdf, fields]"
  ) {
    val testProperties =
      json"""
      {
        "title": {
          "en": "English title",
          "cy": "Welsh title"
        },
        "panelTitle": {
          "en": "English panel title",
          "cy": "Welsh panel title"
        },
        "showReference": true,
        "instructionPdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          }
        },
        "pdf": {
          "header": {
            "en": "English header",
            "cy": "Welsh header"
          },
          "footer": {
            "en": "English footer",
            "cy": "Welsh footer"
          },
          "tabularFormat": true,
          "includeSignatureBox": true
        },
        "fields": []
      }"""

    val jsonTemplate = constructTestAcknowledgementSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [taxPeriodDate]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "taxPeriodDate",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [calendarDate]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "calendarDate",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [date]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "date",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [address]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "address",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [address]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "address",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [number]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "number",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [positiveNumber]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveNumber",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [positiveWholeNumber]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveWholeNumber",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [sterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "sterling",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [positiveSterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveSterling",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the property errorExample is used with the type property set to [text] and the format property set to [positiveWholeSterling]"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "text",
            "format": "positiveWholeSterling",
            "errorExample": {
              "en": "English error example",
              "cy": "Welsh error example"
            }
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a choices property contains an underscored includeIf (_includeIf)"
  ) {
    val testProperties =
      json"""
          {
            "id": "testId",
            "label": "test label",
            "type": "choice",
            "choices": [
              {
                "en": "en choice 1",
                "cy": "cy choice 1",
                "dynamic": "dynamic.one",
                "hint": {
                  "en": "en hint 1",
                  "cy": "cy hint 1"
                },
                "value": "NONE",
                "_includeIf": "1=1"
              },
              {
                "en": "en choice 2"
              }
            ]
          }
        """

    val jsonTemplate = constructTestOneSectionJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task contains the property caption as a string"
  ) {
    val testProperties =
      json"""
          {
            "title": "Test title",
            "caption": "Test caption",
            "sections": []
          }
        """

    val jsonTemplate = constructTestTaskListJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task contains the property caption as a stringOrEnCy object only containing the en key"
  ) {
    val testProperties =
      json"""
          {
            "title": "Test title",
            "caption": {
              "en": "English caption"
            },
            "sections": []
          }
        """

    val jsonTemplate = constructTestTaskListJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task contains the property caption as a stringOrEnCy object containing both the en and cy keys"
  ) {
    val testProperties =
      json"""
          {
            "title": "Test title",
            "caption": {
              "en": "English caption",
              "cy": "Welsh caption"
            },
            "sections": []
          }
        """

    val jsonTemplate = constructTestTaskListJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when a task does not contain the caption property"
  ) {
    val testProperties =
      json"""
          {
            "title": "Test title",
            "sections": []
          }
        """

    val jsonTemplate = constructTestTaskListJsonTemplate(testProperties)

    val result = validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }
}
