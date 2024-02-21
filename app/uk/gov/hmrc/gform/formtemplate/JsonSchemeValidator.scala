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
import io.circe.Decoder.Result
import io.circe.{ DecodingFailure, Json }
import io.circe.jawn.JawnParser
import io.circe.schema.Schema
import io.circe.schema.ValidationError
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

import scala.annotation.tailrec

object JsonSchemeValidator {

  val inputStream = getClass.getClassLoader.getResourceAsStream("formTemplateSchema.json")

  val schemaStream = scala.io.Source.fromInputStream(inputStream).mkString

  val parser = JawnParser(allowDuplicateKeys = false)
  val parsedSchema = parser.parse(schemaStream)

  def checkSchema(json: String): Either[SchemaValidationException, Unit] = parser.parse(json) match {
    case Right(json)          => validateJson(json)
    case Left(parsingFailure) => Left(SchemaValidationException("Json error: " + parsingFailure))
  }

  def validateJson(json: Json): Either[SchemaValidationException, Unit] =
    parsedSchema match {
      case Left(parsingFailure) => Left(SchemaValidationException("Schema error: " + parsingFailure))
      case Right(schema) =>
        val formTemplateSchema: Schema = Schema.load(schema)
        val validated: ValidatedNel[ValidationError, Unit] = formTemplateSchema.validate(json)

        validated.leftMap { errors =>
          val fullError: NonEmptyList[String] = {
            val errorsWithParsedTypeErrors = parseTypeErrors(errors, json)

            maybeFirstSchemaValidationErrorIndex(errors) match {
              case None => errorsWithParsedTypeErrors.map(_.getMessage)
              case Some(firstDependencyIndex) =>
                val conditionalValidationErrors: List[String] =
                  parseConditionalValidationErrors(errors, firstDependencyIndex, schema, json)

                constructFullError(errorsWithParsedTypeErrors, conditionalValidationErrors, firstDependencyIndex)
            }
          }

          SchemaValidationException(fullError)
        }.toEither
    }

  private def parseTypeErrors(
    errors: NonEmptyList[ValidationError],
    json: Json
  ): NonEmptyList[ValidationError] =
    errors.map { error =>
      if (error.keyword == "type") {
        val errorProperty = error.location.split("/").last
        val errorLocationId: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)
        val errorMessage =
          s"Error at ID <$errorLocationId>: Property $errorProperty ${error.getMessage.split(":").tail.mkString(":").strip()}"

        ValidationError(error.keyword, errorMessage, error.location, error.schemaLocation)
      } else {
        error
      }
    }

  private def maybeFirstSchemaValidationErrorIndex(errors: NonEmptyList[ValidationError]): Option[Int] = errors
    .map(_.schemaLocation.getOrElse(""))
    .toList
    .zipWithIndex
    .collectFirst { case (lookup, index) if lookup.contains("dependencies") => index }

  private def parseConditionalValidationErrors(
    errors: NonEmptyList[ValidationError],
    firstDependencyIndex: Int,
    schema: Json,
    json: Json
  ): List[String] =
    getErrorLocationsAndProperties(errors, firstDependencyIndex).distinct.map { case (location, property) =>
      val errorLocation = tryConvertErrorLocationToId(json, location, propertyNameInLocation = false)

      val maybeRequirements: Either[DecodingFailure, String] =
        propertyRequirementsFromSchema(schema, property).map(requiredPropertyValuesFromPattern)

      maybeRequirements match {
        case Left(_) => s"$errorLocation: Could not find validation in the schema for property: $property"
        case Right(requirements) =>
          s"Error at ID <$errorLocation>: Property $property can only be used with $requirements"
      }
    }

  private def getErrorLocationsAndProperties(
    errors: NonEmptyList[ValidationError],
    firstDependencyIndex: Int
  ): List[(String, String)] =
    errors.toList.slice(firstDependencyIndex, errors.length).flatMap { error =>
      error.schemaLocation.flatMap(schemaErrorLocation =>
        maybeErrorLocationAndPropertyFromKeyword(error, schemaErrorLocation.split("/"))
      )
    }

  private def maybeErrorLocationAndPropertyFromKeyword(
    error: ValidationError,
    splitErrorLocation: Array[String]
  ): Option[(String, String)] =
    error.keyword match {
      case "pattern" =>
        Some(
          (
            error.location.substring(0, error.location.lastIndexOf("/")),
            splitErrorLocation(splitErrorLocation.length - 3)
          )
        )
      case "required" =>
        Some((error.location, splitErrorLocation.last))
      case _ => None
    }

  private def tryConvertErrorLocationToId(json: Json, location: String, propertyNameInLocation: Boolean): String = {
    val errorLocationSections = location.split("/").toList.tail
    val errorLocationSectionsWithoutProperty =
      if (propertyNameInLocation) errorLocationSections.dropRight(1) else errorLocationSections

    // If cannot get ID of location, use original location message instead
    getErrorLocationId(json, errorLocationSectionsWithoutProperty).flatMap(_.asString).getOrElse(location)
  }

  @tailrec
  private def getErrorLocationId(json: Json, remainingSections: List[String]): Option[Json] =
    remainingSections match {
      // No more sections to traverse, so get ID of current section to return
      case Nil =>
        json.hcursor.downField("id").as[Json] match {
          case Left(_)                => None
          case Right(errorLocationId) => Some(errorLocationId)
        }

      // More sections to traverse, so check if current json is a List of Json or a Json
      case section :: nextRemainingSections =>
        json.as[List[Json]] match {

          // If Json, go to next section
          case Left(_) =>
            json.hcursor.downField(section).as[Json] match {
              case Left(_)                => None
              case Right(nextJsonSection) => getErrorLocationId(nextJsonSection, nextRemainingSections)
            }

          // If List of Json, get Int of next section and index the List to go to next section
          case Right(jsonList) =>
            section.toIntOption match {
              case Some(sectionInt) => getErrorLocationId(jsonList(sectionInt), nextRemainingSections)
              case None             => None
            }
        }
    }

  private def propertyRequirementsFromSchema(schema: Json, property: String): Result[Map[String, Map[String, String]]] =
    schema.hcursor
      .downField("$defs")
      .downField("fields")
      .downField("dependencies")
      .downField(property)
      .downField("properties")
      .as[Map[String, Map[String, String]]]

  private def requiredPropertyValuesFromPattern(requiredPropertyAndPattern: Map[String, Map[String, String]]): String =
    requiredPropertyAndPattern.view
      .mapValues { requiredPattern =>
        requiredPattern.values
          .map { requiredValues =>
            requiredValues.substring(1, requiredValues.length - 1).replace("|", ", ")
          }
          .mkString(", ")
      }
      .map { case (requiredProperty, requiredValues) =>
        if (requiredValues.contains("?!")) {
          s"$requiredProperty not: [${requiredValues.substring(4, requiredValues.length - 4)}]"
        } else {
          s"$requiredProperty: [$requiredValues]"
        }
      }
      .mkString(", ")

  private def constructFullError(
    baseErrors: NonEmptyList[ValidationError],
    conditionalValidationErrors: List[String],
    firstDependencyIndex: Int
  ) =
    // Using unsafe because errors is an NEL and firstDependencyIndex is not None in this branch
    NonEmptyList.fromListUnsafe(
      baseErrors
        .map(_.getMessage)
        .toList
        .slice(0, firstDependencyIndex) ++ conditionalValidationErrors
    )
}
