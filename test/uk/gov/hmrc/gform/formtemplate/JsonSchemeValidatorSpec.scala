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

import io.circe.{ Json, ParsingFailure }
import io.circe.jawn.JawnParser
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.JsArray
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

import scala.io.Source

class JsonSchemeValidatorSpec extends AnyWordSpec with MockFactory with Matchers {

  val parser = JawnParser(allowDuplicateKeys = false)

  "validateJson" should {

    "reject the form gracefully" when {

      def readInvalidJsonAsFormTemplate(fileName: String): Either[ParsingFailure, Json] = {
        val source = Source.fromFile(s"test/resources/templates-to-reject/$fileName.json").mkString
        parser.parse(source)
      }

      def runInvalidJsonTest(fileName: String, expectedResult: List[String]) = {
        val invalidFile = readInvalidJsonAsFormTemplate(fileName)

        invalidFile match {
          case Left(error: ParsingFailure) =>
            fail("Json failed to parse before validation took place, with error:\n" + error)

          case Right(json: Json) =>
            JsonSchemeValidator.validateJson(json) match {
              case Left(errors: SchemaValidationException) =>
                val errorsAsList = errors.errors.as[JsArray].value.toList

                // Check that the number of custom conditional validation errors matches the expected number
                errorsAsList
                  .map(_.toString().contains("can only be used with"))
                  .count(_ == true) shouldBe expectedResult.length

                // Only checking last errors because conditional validation messages will appear at the bottom of the stack trace
                val conditionalValidationErrors =
                  errorsAsList.slice(errorsAsList.length - expectedResult.length, errorsAsList.length)

                conditionalValidationErrors shouldBe expectedResult.map(toJson(_))

              case Right(value) => fail("No error was returned from schema validation:\n" + value)
            }
        }
      }

      "a single property has a dependency error" when {

        "the property infoType is used when the type property is not [info]" in {
          runInvalidJsonTest(
            fileName = "infoType-with-type-not-info",
            expectedResult = List("#/sections/0/fields/0: Property infoType can only be used with type: [info]")
          )
        }

        "the property infoText is used when the type property is not [info]" in {
          runInvalidJsonTest(
            fileName = "infoText-with-type-not-info",
            expectedResult = List("#/sections/0/fields/0: Property infoText can only be used with type: [info]")
          )
        }

        "the property choices is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "choices-with-invalid-type",
            expectedResult =
              List("#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]")
          )
        }

        "the property multivalue is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "multivalue-with-invalid-type",
            expectedResult =
              List("#/sections/0/fields/0: Property multivalue can only be used with type: [choice, revealingChoice]")
          )
        }

        "the property hints is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "hints-with-invalid-type",
            expectedResult =
              List("#/sections/0/fields/0: Property hints can only be used with type: [choice, revealingChoice]")
          )
        }

        "the property optionHelpText is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "optionHelpText-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property optionHelpText can only be used with type: [choice, revealingChoice]"
            )
          )
        }

        "the property dividerPosition is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "dividerPosition-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property dividerPosition can only be used with type: [choice, revealingChoice]"
            )
          )
        }

        "the property noneChoice is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "noneChoice-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property noneChoice can only be used with type: [choice, revealingChoice]"
            )
          )
        }

        "the property noneChoiceError is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "noneChoiceError-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property noneChoiceError can only be used with type: [choice, revealingChoice]"
            )
          )
        }

        "the property dividerText is used when the type property is not either of [choice, revealingChoice]" in {
          runInvalidJsonTest(
            fileName = "dividerText-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property dividerText can only be used with type: [choice, revealingChoice]"
            )
          )
        }

        "the property displayCharCount is used when the type property is not [text]" in {
          runInvalidJsonTest(
            fileName = "displayCharCount-with-invalid-type",
            expectedResult = List(
              "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
            )
          )
        }

        "the property displayCharCount is used when the multiline property is not [true]" in {
          runInvalidJsonTest(
            fileName = "displayCharCount-with-invalid-multiline",
            expectedResult = List(
              "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
            )
          )
        }

        "the property displayCharCount is used when the type property is not [text] and the multiline property is not [true]" in {
          runInvalidJsonTest(
            fileName = "displayCharCount-with-invalid-multiline",
            expectedResult = List(
              "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]"
            )
          )
        }
      }

      "multiple properties have dependency errors" when {

        "infoText and infoType are used when the type property is not [info]" in {
          runInvalidJsonTest(
            fileName = "infoText-and-infoType-with-type-not-info",
            expectedResult = List(
              "#/sections/0/fields/0: Property infoText can only be used with type: [info]",
              "#/sections/0/fields/0: Property infoType can only be used with type: [info]"
            )
          )
        }

        "infoText and infoType are used in different sections when the type property is not [info]" in {
          runInvalidJsonTest(
            fileName = "infoText-and-infoType-different-sections-with-type-not-info",
            expectedResult = List(
              "#/sections/0/fields/0: Property infoText can only be used with type: [info]",
              "#/sections/1/fields/0: Property infoType can only be used with type: [info]"
            )
          )
        }

        "all properties dependent on type property being [choice, revealingChoice] are used in one section with invalid type" in {
          runInvalidJsonTest(
            fileName = "all-properties-dependent-on-type-choice-revealingChoice",
            expectedResult = List(
              "#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property multivalue can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property hints can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property optionHelpText can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property dividerPosition can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property noneChoice can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property noneChoiceError can only be used with type: [choice, revealingChoice]",
              "#/sections/0/fields/0: Property dividerText can only be used with type: [choice, revealingChoice]"
            )
          )
        }
      }

      """displayCharCount and dataThreshold are used in different sections when the type property is not [info]
        | in one section and the multiline property is not [true] in the other""".stripMargin in {
        runInvalidJsonTest(
          fileName = "displayCharCount-and-dataThreshold-with-invalid-dependencies",
          expectedResult = List(
            "#/sections/0/fields/0: Property displayCharCount can only be used with type: [text], multiline: [true]",
            "#/sections/1/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
          )
        )
      }

      "properties with different dependencies all have errors" in {
        runInvalidJsonTest(
          fileName = "multiple-properties-with-conditional-validation-errors",
          expectedResult = List(
            "#/sections/0/fields/0: Property infoType can only be used with type: [info]",
            "#/sections/0/fields/0: Property choices can only be used with type: [choice, revealingChoice]",
            "#/sections/0/fields/0: Property dataThreshold can only be used with type: [text], multiline: [true]"
          )
        )
      }
    }

    "accept the form" when {

      def readValidJsonAsFormTemplate(fileName: String): Either[ParsingFailure, Json] = {
        val source = Source.fromFile(s"test/resources/templates-to-normalise/$fileName.json").mkString
        parser.parse(source)
      }

      def runValidJsonTest(fileName: String) = {
        val validFile = readValidJsonAsFormTemplate(fileName)

        validFile match {
          case Left(error: ParsingFailure) =>
            fail("Json failed to parse before validation took place, with error:\n" + error)

          case Right(json: Json) =>
            JsonSchemeValidator.validateJson(json) shouldBe Right(())
        }
      }

      "the type property is [info] and the properties that are dependent on this are present" in {
        runValidJsonTest(fileName = "conditional-with-valid-type-info")
      }

      "the type property is [choice] and the properties that are dependent on this are present" in {
        runValidJsonTest(fileName = "conditional-with-valid-type-choice")
      }

      "the type property is [revealingChoice] and the properties that are dependent on this are present" in {
        runValidJsonTest(fileName = "conditional-with-valid-type-revealingChoice")
      }

      "the type property is [text], the multiline property is [true], and the properties that are dependent on these are present" in {
        runValidJsonTest(fileName = "conditional-with-valid-type-text-and-multiline-true")
      }
    }
  }
}
