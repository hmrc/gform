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

import cats.data.NonEmptyList
import io.circe.{ DecodingFailure, Json }
import io.circe.schema.ValidationError
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

import scala.annotation.tailrec

object JsonSchemeErrorParser {

  def parseErrorMessages(errors: NonEmptyList[ValidationError], schema: Json, json: Json): SchemaValidationException = {
    val parsedErrors: NonEmptyList[ValidationError] = errors.map { error =>
      val parsedErrorMessage: String =
        if (error.schemaLocation.getOrElse("").contains("dependencies")) {
          parseConditionalValidationErrorMessage(schema, json, error)
        } else if (errors.filter(_.location == error.location).map(_.keyword).contains("type")) {
          parseTypeError(error, errors, json, schema)
        } else {
          error.getMessage
        }

      ValidationError(error.keyword, parsedErrorMessage, error.location, error.schemaLocation)
    }

    SchemaValidationException(parsedErrors.map(_.getMessage).distinct)
  }

  private def parseConditionalValidationErrorMessage(schema: Json, json: Json, error: ValidationError): String =
    error.keyword match {
      case "pattern" =>
        parsePatternConditionalValidationError(schema, json, error)

      case "required" =>
        parseRequiredConditionalValidationError(schema, json, error)

      case _ => error.getMessage
    }

  private def parsePatternConditionalValidationError(schema: Json, json: Json, error: ValidationError): String = {
    val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)

    val maybeRequirements: Either[DecodingFailure, Json] =
      goDownSchema(
        schema,
        error.schemaLocation.getOrElse("").split("/").tail.toList.dropRight(1)
      )

    val property: String = error.schemaLocation
      .getOrElse("")
      .split("/")(error.schemaLocation.getOrElse("").split("/").length - 3)

    getErrorMessageFromConditionalRequirements(maybeRequirements, error, errorLocation, property)
  }

  private def parseRequiredConditionalValidationError(schema: Json, json: Json, error: ValidationError): String = {
    val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = false)

    val maybeRequirements: Either[DecodingFailure, Json] =
      goDownSchema(
        schema,
        error.schemaLocation.getOrElse("").split("/").tail.toList ++ List("properties")
      )

    val property: String = error.schemaLocation
      .getOrElse("")
      .split("/")
      .last

    getErrorMessageFromConditionalRequirements(maybeRequirements, error, errorLocation, property)
  }

  private def tryConvertErrorLocationToId(json: Json, location: String, propertyNameInLocation: Boolean): String = {
    val errorLocationSections: List[String] = location.split("/").tail.toList
    val errorLocationSectionsWithoutProperty: List[String] =
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

  @tailrec
  private def goDownSchema(schema: Json, remaining: List[String]): Either[DecodingFailure, Json] =
    remaining match {
      case Nil => Right(schema)
      case current :: next =>
        schema.hcursor
          .downField(current)
          .as[Json] match {
          case Left(decodingFailure)  => Left(decodingFailure)
          case Right(reducedSchema) => goDownSchema(reducedSchema, next)
        }
    }

  private def getErrorMessageFromConditionalRequirements(
    maybeRequirements: Either[DecodingFailure, Json],
    error: ValidationError,
    errorLocation: String,
    property: String
  ): String =
    maybeRequirements match {
      case Left(_) => error.getMessage
      case Right(requirements: Json) =>
        requirements.as[Map[String, Map[String, String]]] match {
          case Left(_) => error.getMessage
          case Right(propertyAndPattern) =>
            val errorMessage: String =
              s"Property $property can only be used with ${requiredPropertyValuesFromPattern(propertyAndPattern)}"
            constructCustomErrorMessage(errorLocation, errorMessage)
        }
    }

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

  private def constructCustomErrorMessage(errorLocation: String, errorMessage: String): String =
    s"Error at ${if (!errorLocation.startsWith("#")) "ID " else ""}<$errorLocation>: $errorMessage"

  private def parseTypeError(
    error: ValidationError,
    errors: NonEmptyList[ValidationError],
    json: Json,
    schema: Json
  ): String = {
    val allSameErrors: List[ValidationError] = errors.filter(_.location == error.location)
    if (allSameErrors.map(_.schemaLocation).exists(_.contains("#/$defs/stringOrEnCyObject"))) {
      parseEnCyTypeError(allSameErrors, json, error)
    } else {
      parseNormalTypeError(schema, json, error, errors)
    }
  }

  private def parseEnCyTypeError(allSameErrors: List[ValidationError], json: Json, error: ValidationError) = {
    val maybeInvalidKeyMessage: Option[String] = keysFromErrorMessage(allSameErrors, "additionalProperties") match {
      case Nil         => None
      case invalidKeys => Some(s"Invalid key(s) [${invalidKeys.mkString(", ")}] are not permitted")
    }

    val maybeRequiredKeyMessage: Option[String] = keysFromErrorMessage(allSameErrors, "required") match {
      case Nil          => None
      case requiredKeys => Some(s"Missing key(s) [${requiredKeys.mkString(", ")}] are required")
    }

    List(Some(constructEnCyTypeError(json, error)), maybeRequiredKeyMessage, maybeInvalidKeyMessage).flatten
      .mkString(". ")
  }

  private def keysFromErrorMessage(allSameErrors: List[ValidationError], keyword: String): List[String] =
    allSameErrors
      .filter(_.keyword == keyword)
      .map(_.getMessage)
      .map("\\[.*]".r.findAllIn(_).next().drop(1).dropRight(1))

  private def constructEnCyTypeError(json: Json, error: ValidationError): String = {
    val errorProperty: String = error.location.split("/").last
    val errorLocation: String =
      tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)

    val errorMessage: String =
      s"Property $errorProperty Expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    constructCustomErrorMessage(errorLocation, errorMessage)
  }

  private def parseNormalTypeError(
    schema: Json,
    json: Json,
    error: ValidationError,
    errors: NonEmptyList[ValidationError]
  ): String = {
    val maybeSchemaError: List[Option[String]] = errors
      .filter(_.location == error.location)
      .map(_.schemaLocation)
      .filter(_.isDefined)
      .distinct

    maybeSchemaError match {
      case maybeSchemaLocation :: Nil =>
        maybeSchemaLocation match {
          case Some(schemaLocation) =>
            generateNormalTypeErrorMessage(schema, json, error, errors, schemaLocation)
          case None => error.getMessage
        }
      case _ => error.getMessage
    }
  }

  private def generateNormalTypeErrorMessage(
    schema: Json,
    json: Json,
    error: ValidationError,
    errors: NonEmptyList[ValidationError],
    schemaLocation: String
  ): String = {
    val errorProperty: String = error.location.split("/").last
    val errorLocation: String =
      tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)

    errors.filter(_.location == error.location).filter(_.keyword == "type").map(_.getMessage) match {
      case Nil => error.getMessage
      case errorMessage :: Nil =>
        val typeFound: String = errorMessage.split(" ").last
        constructNormalTypeErrorMessage(schema, schemaLocation, error, errorLocation, errorProperty, typeFound)
      case _ => error.getMessage
    }
  }

  private def constructNormalTypeErrorMessage(
    schema: Json,
    schemaLocation: String,
    error: ValidationError,
    errorLocation: String,
    errorProperty: String,
    typeFound: String
  ): String =
    goDownSchema(schema, schemaLocation.split("/").toList.tail ++ List("type")) match {
      case Left(_) => error.getMessage

      case Right(requiredType) =>
        val maybeRequiredType: Option[String] = if (requiredType.isArray) {
          requiredType.as[List[String]].map(_.mkString(", ")).toOption
        } else if (requiredType.isString) {
          requiredType.asString
        } else {
          None
        }

        maybeRequiredType match {
          case Some(requiredType) =>
            val errorMessage: String =
              s"Property $errorProperty expected type [${requiredType.capitalize}], found [$typeFound]"
            constructCustomErrorMessage(errorLocation, errorMessage)
          case None => error.getMessage
        }
    }
}
