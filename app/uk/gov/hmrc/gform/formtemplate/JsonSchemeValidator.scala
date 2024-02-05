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

package uk.gov.hmrc.gform.formtemplate

import cats.data.{ NonEmptyList, ValidatedNel }
import io.circe.Json
import io.circe.jawn.JawnParser
import io.circe.schema.Schema
import io.circe.schema.ValidationError
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

object JsonSchemeValidator {

  val inputStream = getClass.getClassLoader.getResourceAsStream("formTemplateSchema.json")

  val schemaStream = scala.io.Source.fromInputStream(inputStream).mkString

  val parser = JawnParser(allowDuplicateKeys = false)
  val parsedSchema = parser.parse(schemaStream)

  private val conditionalRequirements: Map[String, List[ConditionalValidationRequirement]] = Map(
    "infoType"         -> List(TypeInfo),
    "infoText"         -> List(TypeInfo),
    "choices"          -> List(TypeChoiceOrRevealingChoice),
    "multivalue"       -> List(TypeChoiceOrRevealingChoice),
    "hints"            -> List(TypeChoiceOrRevealingChoice),
    "optionHelpText"   -> List(TypeChoiceOrRevealingChoice),
    "dividerPosition"  -> List(TypeChoiceOrRevealingChoice),
    "noneChoice"       -> List(TypeChoiceOrRevealingChoice),
    "noneChoiceError"  -> List(TypeChoiceOrRevealingChoice),
    "dividerText"      -> List(TypeChoiceOrRevealingChoice),
    "displayCharCount" -> List(TypeText, MultiLineTrueOrYes),
    "dataThreshold"    -> List(TypeText, MultiLineTrueOrYes)
  )

  def checkSchema(json: String): Either[SchemaValidationException, Unit] = parser.parse(json) match {
    case Right(json)          => validateJson(json)
    case Left(parsingFailure) => Left(SchemaValidationException("Json error: " + parsingFailure))
  }

  private def validateJson(json: Json): Either[SchemaValidationException, Unit] =
    parsedSchema match {
      case Left(parsingFailure) => Left(SchemaValidationException("Schema error: " + parsingFailure))
      case Right(schema) =>
        val formTemplateSchema: Schema = Schema.load(schema)
        val validated: ValidatedNel[ValidationError, Unit] = formTemplateSchema.validate(json)

        validated.leftMap { errors =>
          val conditionalValidationErrorMessages: List[String] =
            if (errors.map(_.keyword).toList.toSet.contains("anyOf")) {
              errors
                .map { error =>
                  error.keyword match {
                    case "not" =>
                      val errorField: String = "\\[\".+\"]".r.findAllIn(error.getMessage).next()
                      Some(errorField.slice(2, errorField.length - 2))
                    case "pattern" => Some(error.location)
                    case _         => None
                  }
                }
                .toList
                .flatten
                .grouped(2)
                .toList
                .map { field =>
                  s"${field(1)}: Property ${field.head} is only valid for ${conditionalRequirements(field.head).mkString(", ")}"
                }
            } else {
              errors.map(_.getMessage).toList
            }
          println(conditionalValidationErrorMessages)
          SchemaValidationException(
            NonEmptyList(conditionalValidationErrorMessages.head, conditionalValidationErrorMessages.tail)
          )
        }.toEither
    }

}
