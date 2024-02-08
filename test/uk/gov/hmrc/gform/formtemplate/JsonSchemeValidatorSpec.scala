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
import io.circe.syntax.EncoderOps
import munit.FunSuite
import play.api.libs.json.{ JsArray, JsValue }
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

class JsonSchemeValidatorSpec extends FunSuite {

  private def constructTestOneSectionJsonTemplate(
    propertyDependencies: Map[String, String],
    strings: Map[String, String],
    ints: Map[String, Int],
    arraysOfStrings: Map[String, Array[String]],
    arraysOfObjects: Map[String, Array[Map[String, String]]],
    objects: Map[String, Map[String, String]]
  ): Json = {
    val basePropertiesAndValues = Map(
      "id"    -> "TestID",
      "label" -> "Test Label"
    )

    val allFields = basePropertiesAndValues.asJson
      .deepMerge(propertyDependencies.asJson)
      .deepMerge(strings.asJson)
      .deepMerge(ints.asJson)
      .deepMerge(arraysOfStrings.asJson)
      .deepMerge(arraysOfObjects.asJson)
      .deepMerge(objects.asJson)

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
               $allFields
            ]
          }
        ]
      }"""
  }

  private def getNumberOfErrors(errorsAsList: List[JsValue]): Int =
    // Get the number of custom conditional validation error messages
    errorsAsList
      .map(_.toString().contains("can only be used with"))
      .count(_ == true)

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
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map("infoType" -> "noformat"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List("#/sections/0/fields/0: Property infoType can only be used with type: [info]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property infoText is used and the type property is not [info]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "choice"),
      strings = Map("infoText" -> "Test infoText"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List("#/sections/0/fields/0: Property infoText can only be used with type: [info]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property choices is used and the type property is not [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map(),
      arraysOfStrings = Map("choices" -> Array("A", "B")),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property multivalue is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "info"),
      strings = Map("multivalue" -> "true"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("#/sections/0/fields/0: Property multivalue can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property hints is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map(),
      arraysOfStrings = Map("hints" -> Array("Test hint 1", "Test hint 2")),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult =
      List("#/sections/0/fields/0: Property hints can only be used with type: [choice, revealingChoice]")

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property optionHelpText is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects =
        Map("optionHelpText" -> Array(Map("en" -> "Test English optionHelpText", "cy" -> "Test Welsh optionHelpText"))),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property optionHelpText can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dividerPosition is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map("dividerPosition" -> 1),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dividerPosition can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dividerText is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects =
        Map("dividerText" -> Array(Map("en" -> "Test English dividerText", "cy" -> "Test Welsh dividerText"))),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dividerText can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property noneChoice is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map("noneChoice" -> 1),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property noneChoice can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property noneChoiceError is used when the type property is not either of [choice, revealingChoice]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(
        "noneChoiceError" -> Array(Map("en" -> "Test English noneChoiceError", "cy" -> "Test Welsh noneChoiceError"))
      ),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property noneChoiceError can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the type property is not [text]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "info", "multiline" -> "true"),
      strings = Map("displayCharCount" -> "false"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the multiline property is not [true]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text", "multiline" -> "false"),
      strings = Map("displayCharCount" -> "false"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the multiline property is not present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map("displayCharCount" -> "false"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property displayCharCount is used when the type property is not [text] and the multiline property is not [true]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "choice", "multiline" -> "false"),
      strings = Map("displayCharCount" -> "false"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the type property is not [text]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "info", "multiline" -> "true"),
      strings = Map(),
      ints = Map("dataThreshold" -> 75),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the multiline property is not [true]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text", "multiline" -> "false"),
      strings = Map(),
      ints = Map("dataThreshold" -> 75),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the multiline property is not present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map(),
      ints = Map("dataThreshold" -> 75),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when the property dataThreshold is used when the type property is not [text] and the multiline property is not [true]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "choice", "multiline" -> "false"),
      strings = Map(),
      ints = Map("dataThreshold" -> 75),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in one section and the type property is not [info]"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map("infoText" -> "Test infoText", "infoType" -> "noformat"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property infoText can only be used with type: [info]",
      "#/sections/0/fields/0: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when all properties dependent on type property being [choice, revealingChoice] are used in one section with invalid type"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map("multivalue" -> "true"),
      ints = Map("dividerPosition" -> 1, "noneChoice" -> 1),
      arraysOfStrings = Map("choices" -> Array("A", "B"), "hints" -> Array("Test hint 1", "Test hint 2")),
      arraysOfObjects =
        Map("optionHelpText" -> Array(Map("en" -> "Test English optionHelpText", "cy" -> "Test Welsh optionHelpText"))),
      objects = Map(
        "dividerText"     -> Map("en" -> "Test English dividerText", "cy" -> "Test Welsh dividerText"),
        "noneChoiceError" -> Map("en" -> "Test English noneChoiceError", "cy" -> "Test Welsh noneChoiceError")
      )
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property multivalue can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property hints can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property optionHelpText can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property dividerPosition can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property noneChoice can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property noneChoiceError can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property dividerText can only be used with type: [choice, revealingChoice]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when properties with different dependencies all have errors"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text"),
      strings = Map("infoType" -> "noformat"),
      ints = Map("dataThreshold" -> 1),
      arraysOfStrings = Map("choices" -> Array("A", "B")),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property infoType can only be used with type: [info]",
      "#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]",
      "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when infoText and infoType are used in different sections when the type property is not [info]"
  ) {
    val jsonTemplate =
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
              {
                "id": "TestID1",
                "label": "Test label 1",
                "type": "text",
                "infoText": "Test infoText"
              }
            ]
          },
          {
            "title": "Page 2",
            "fields": [
              {
                "id": "TestID2",
                "label": "Test label 2",
                "type": "choice",
                "infoType": "noformat"
              }
            ]
          }
        ]
      }"""

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property infoText can only be used with type: [info]",
      "#/sections/1/fields/0: Property infoType can only be used with type: [info]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson rejects the form gracefully when displayCharCount and dataThreshold are used in different sections when the type property is not [info] in one section and the multiline property is not[true] in the other"
  ) {
    val jsonTemplate =
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
              {
                "id": "TestID1",
                "label": "Test label 1",
                "type": "text",
                "displayCharCount": "false"
              }
            ]
          },
          {
            "title": "Page 2",
            "fields": [
              {
                "id": "TestID2",
                "label": "Test label 2",
                "type": "info",
                "multiline": "true",
                "dataThreshold": 1
              }
            ]
          }
        ]
      }"""

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = List(
      "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]",
      "#/sections/1/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
    )

    runInvalidJsonTest(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [info] and the properties that are dependent on this are present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "info"),
      strings = Map("infoText" -> "Test infoText", "infoType" -> "noformat"),
      ints = Map(),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [choice] and the properties that are dependent on this are present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "choice"),
      strings = Map("multivalue" -> "true"),
      ints = Map("dividerPosition" -> 1, "noneChoice" -> 1),
      arraysOfStrings = Map("choices" -> Array("A", "B"), "hints" -> Array("Test hint 1", "Test hint 2")),
      arraysOfObjects =
        Map("optionHelpText" -> Array(Map("en" -> "Test English optionHelpText", "cy" -> "Test Welsh optionHelpText"))),
      objects = Map(
        "dividerText"     -> Map("en" -> "Test English dividerText", "cy" -> "Test Welsh dividerText"),
        "noneChoiceError" -> Map("en" -> "Test English noneChoiceError", "cy" -> "Test Welsh noneChoiceError")
      )
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [revealingChoice] and the properties that are dependent on this are present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "revealingChoice"),
      strings = Map("multivalue" -> "true"),
      ints = Map("dividerPosition" -> 1, "noneChoice" -> 1),
      arraysOfStrings = Map("choices" -> Array("A", "B"), "hints" -> Array("Test hint 1", "Test hint 2")),
      arraysOfObjects =
        Map("optionHelpText" -> Array(Map("en" -> "Test English optionHelpText", "cy" -> "Test Welsh optionHelpText"))),
      objects = Map(
        "dividerText"     -> Map("en" -> "Test English dividerText", "cy" -> "Test Welsh dividerText"),
        "noneChoiceError" -> Map("en" -> "Test English noneChoiceError", "cy" -> "Test Welsh noneChoiceError")
      )
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }

  test(
    "validateJson accepts the form when the type property is [info] and the properties that are dependent on this are present"
  ) {
    val jsonTemplate = constructTestOneSectionJsonTemplate(
      propertyDependencies = Map("type" -> "text", "multiline" -> "true"),
      strings = Map("displayCharCount" -> "false"),
      ints = Map("dataThreshold" -> 1),
      arraysOfStrings = Map(),
      arraysOfObjects = Map(),
      objects = Map()
    )

    val result = JsonSchemeValidator.validateJson(jsonTemplate)

    val expectedResult = Right(())

    assertEquals(result, expectedResult)
  }
}
