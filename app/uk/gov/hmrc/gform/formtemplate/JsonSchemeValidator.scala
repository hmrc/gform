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
import io.circe.Json
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
      case Right(schema: Json) =>
        val formTemplateSchema: Schema = Schema.load(schema)
        val validated: ValidatedNel[ValidationError, Unit] = formTemplateSchema.validate(json)

        validated.leftMap { errors =>
          val fullError: NonEmptyList[String] = {
            maybeFirstSchemaValidationErrorIndex(errors) match {
              case None => errors.map(_.getMessage)
              case Some(firstDependencyIndex) =>
                val conditionalValidationErrors: List[String] =
                  parseConditionalValidationErrors(errors, firstDependencyIndex, schema, json)

                constructFullError(errors, conditionalValidationErrors, firstDependencyIndex)
            }
          }

          SchemaValidationException(fullError)
        }.toEither
    }

  @tailrec
  private def getErrorLocationId(json: Json, remainingSections: List[String]): Option[Json] =
    remainingSections.headOption match {
      // No more sections to traverse, so get ID of current section to return
      case None =>
        json.hcursor.downField("id").as[Json] match {
          case Left(_)                      => None
          case Right(errorLocationId: Json) => Some(errorLocationId)
        }

      // More sections to traverse, so check if current json is a List of Json or a Json
      case Some(section: String) =>
        json.as[List[Json]] match {

          // If Json, go to next section
          case Left(_) =>
            json.hcursor.downField(section).as[Json] match {
              case Left(_)                      => None
              case Right(nextJsonSection: Json) => getErrorLocationId(nextJsonSection, remainingSections.tail)
            }

          // If List of Json, get Int of next section and index the List to go to next section
          case Right(jsonList: List[Json]) =>
            section.toIntOption match {
              case Some(sectionInt: Int) => getErrorLocationId(jsonList(sectionInt), remainingSections.tail)
              case None                  => None
            }
        }
    }

  private def tryConvertErrorLocationToId(json: Json, location: String): String = {
    val errorLocationSections = location.split("/").toList.tail

    getErrorLocationId(json, errorLocationSections) match {
      // If cannot get ID of location, use original location message instead
      case None => location

      case Some(errorLocationId: Json) =>
        errorLocationId.asString match {
          case None                                => location
          case Some(errorLocationIdString: String) => errorLocationIdString
        }
    }
  }

  private def parseConditionalValidationErrors(
    errors: NonEmptyList[ValidationError],
    firstDependencyIndex: Int,
    schema: Json,
    json: Json
  ): List[String] =
    getErrorLocationsAndProperties(errors, firstDependencyIndex).distinct.map { case (location, property) =>
      val errorLocation = tryConvertErrorLocationToId(json, location)

      val maybeRequirements: Option[String] =
        propertyRequirementsFromSchema(schema: Json, property: String) match {
          case Left(_) => None
          case Right(requiredPropertyAndPattern) =>
            Some(requiredPropertyValuesFromPattern(requiredPropertyAndPattern))
        }

      maybeRequirements match {
        case None => s"$errorLocation: Could not find validation in the schema for property: $property"
        case Some(requirements: String) =>
          s"Error at ID <$errorLocation>: Property $property can only be used with $requirements"
      }
    }

  private def maybeFirstSchemaValidationErrorIndex(errors: NonEmptyList[ValidationError]): Option[Int] = errors
    .map(_.schemaLocation.getOrElse(""))
    .toList
    .zipWithIndex
    .filter(_._1.contains("dependencies"))
    .map(_._2)
    .headOption

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

  private def getErrorLocationsAndProperties(
    errors: NonEmptyList[ValidationError],
    firstDependencyIndex: Int
  ): List[(String, String)] =
    errors.toList.slice(firstDependencyIndex, errors.length).flatMap { error =>
      val schemaErrorLocation = error.schemaLocation.getOrElse("")
      schemaErrorLocation match {
        case "" => None
        case _  => maybeErrorLocationAndPropertyFromKeyword(error, schemaErrorLocation.split("/"))
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
    requiredPropertyAndPattern
      .map { case (requiredProperty, pattern) =>
        requiredProperty -> pattern
          .map { case (_, requiredValues) =>
            requiredValues.substring(1, requiredValues.length - 1).replace("|", ", ")
          }
          .mkString(", ")
      }
      .map { case (requiredProperty, requiredValues) =>
        s"$requiredProperty: [$requiredValues]"
      }
      .mkString(", ")

  private def constructFullError(
    baseErrors: NonEmptyList[ValidationError],
    conditionalValidationErrors: List[String],
    firstDependencyIndex: Int
  ) = {

    val typeErrors =
      baseErrors.toList.slice(firstDependencyIndex, baseErrors.length).filter(_.keyword == "type").map(_.getMessage)

    // Using unsafe because errors is an NEL and firstDependencyIndex is not None in this branch
    NonEmptyList.fromListUnsafe(
      baseErrors
        .map(_.getMessage)
        .toList
        .slice(0, firstDependencyIndex) ++ typeErrors ++ conditionalValidationErrors
    )
  }
}
