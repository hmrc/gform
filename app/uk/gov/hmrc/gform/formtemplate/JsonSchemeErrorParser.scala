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
import cats.syntax.eq._
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import io.circe.{ DecodingFailure, Json }
import io.circe.schema.ValidationError
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

import scala.annotation.tailrec

object JsonSchemeErrorParser {

  def parseErrorMessages(errors: NonEmptyList[ValidationError], schema: Json, json: Json): SchemaValidationException = {
    val parsedErrors: NonEmptyList[ValidationError] = errors.map { error =>
      val parsedErrorMessage: String =
        if (error.schemaLocation.getOrElse("").contains("dependencies")) {
          if (error.schemaLocation.getOrElse("").contains("oneOf")) {
            parseConditionalValidationWithOneOfErrorMessage(schema, json, error)
          } else {
            parseConditionalValidationErrorMessage(schema, json, error)
          }
        } else if (errors.filter(_.location === error.location).map(_.keyword).contains("type")) {
          parseTypeError(error, errors, json, schema)
        } else if (error.keyword === "required") {
          parseRequiredError(schema, json, error)
        } else if (error.keyword === "pattern") {
          parsePatternError(schema, json, error)
        } else if (error.keyword === "additionalProperties") {
          parseAdditionalPropertiesError(errors, json, error)
        } else {
          error.getMessage
        }

      ValidationError(error.keyword, parsedErrorMessage, error.location, error.schemaLocation)
    }

    SchemaValidationException(parsedErrors.map(_.getMessage).distinct)
  }

  private def parseRequiredError(schema: Json, json: Json, error: ValidationError): String = {
    val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)

    val maybeRequirements: Either[DecodingFailure, Json] =
      goDownSchema(
        schema,
        error.schemaLocation.getOrElse("").split("/").tail.toList ++ List("required")
      )

    val property: String = error.schemaLocation
      .getOrElse("")
      .split("/")
      .last

    maybeRequirements match {
      case Left(_) => error.getMessage
      case Right(requirementsJson) =>
        requirementsJson.as[List[String]] match {
          case Left(_) => error.getMessage
          case Right(requirements) =>
            val errorMessage = s"$property requires properties [${requirements.mkString(", ")}] to be present"
            constructCustomErrorMessage(errorLocation, errorMessage)
        }
    }
  }

  private def parsePatternError(schema: Json, json: Json, error: ValidationError): String = {
    val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = false)

    val maybeRequirements: Either[DecodingFailure, Json] =
      goDownSchema(
        schema,
        error.schemaLocation.getOrElse("").split("/").tail.toList ++ List("pattern")
      )

    val property: String = error.schemaLocation
      .getOrElse("")
      .split("/")
      .last

    maybeRequirements match {
      case Left(_) => error.getMessage
      case Right(requirementsJson) =>
        requirementsJson.asString match {
          case None => error.getMessage
          case Some(requiredValues) =>
            val notMatchingPattern: Boolean = requiredValues.contains("?!")
            val lengthOfTrim: Int = if (notMatchingPattern) 5 else 2

            val parsedRequiredValues: String =
              requiredValues.substring(lengthOfTrim, requiredValues.length - lengthOfTrim).replace("|", ", ")

            val errorMessage: String =
              s"Property $property expected value ${if (notMatchingPattern) "not " else ""}[$parsedRequiredValues]"

            constructCustomErrorMessage(errorLocation, errorMessage)
        }
    }
  }

  private def parseAdditionalPropertiesError(
    errors: NonEmptyList[ValidationError],
    json: Json,
    error: ValidationError
  ): String = {
    val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = false)
    val allSameErrors = errors.filter(_.location === error.location).filter(_.keyword === error.keyword)

    val property: String = error.schemaLocation
      .getOrElse("")
      .split("/")
      .last

    val allInvalidProperties = allSameErrors
      .map(_.getMessage)
      .map("\\[.*]".r.findAllIn(_).next())
      .map(property => property.substring(1, property.length - 1))
      .mkString(", ")

    val errorMessage = s"$property has invalid key(s) [$allInvalidProperties]"
    constructCustomErrorMessage(errorLocation, errorMessage)
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
    val (maybeFoundId, restOfLocation) =
      getErrorLocationIdWithRemainingSections(json, errorLocationSectionsWithoutProperty, (None, ""))

    maybeFoundId.flatMap(_.asString) match {
      case None          => location
      case Some(foundId) => if (restOfLocation.nonEmpty) s"$foundId: $restOfLocation".stripTrailing() else foundId
    }
  }

  @tailrec
  private def getErrorLocationIdWithRemainingSections(
    json: Json,
    remainingSections: List[String],
    maybeFoundLocationAndSections: (Option[Json], String)
  ): (Option[Json], String) = {
    val maybeFoundId: Option[Json] = json.hcursor.downField("id").as[Json].toOption

    remainingSections match {
      // No more sections to traverse, so return current section ID if defined, else previously found ID
      case Nil =>
        if (maybeFoundId.isDefined) (maybeFoundId, "") else maybeFoundLocationAndSections

      // More sections to traverse, so check if current json is a List of Json or a Json
      case section :: nextRemainingSections =>
        // Keep track of lowest level ID and remaining sections found
        val newLocationAndSections: (Option[Json], String) = if (maybeFoundId.isDefined) {
          (maybeFoundId, remainingSections.mkString("/"))
        } else {
          maybeFoundLocationAndSections
        }

        json.as[List[Json]] match {

          // If Json, go to next section
          case Left(_) =>
            json.hcursor.downField(section).as[Json] match {
              case Left(_) => (None, "")
              case Right(nextJsonSection) =>
                getErrorLocationIdWithRemainingSections(nextJsonSection, nextRemainingSections, newLocationAndSections)
            }

          // If List of Json, get Int of next section and index the List to go to next section
          case Right(jsonList) =>
            section.toIntOption match {
              case Some(sectionInt) =>
                getErrorLocationIdWithRemainingSections(
                  jsonList(sectionInt),
                  nextRemainingSections,
                  newLocationAndSections
                )
              case None => (None, "")
            }
        }
    }
  }

  @tailrec
  private def goDownSchema(schema: Json, remaining: List[String]): Either[DecodingFailure, Json] =
    remaining match {
      case Nil => Right(schema)
      case current :: next =>
        schema.as[List[Json]] match {

          case Left(_) =>
            schema.hcursor.downField(current).as[Json] match {
              case Left(decodingFailure) => Left(decodingFailure)
              case Right(reducedSchema) =>
                goDownSchema(reducedSchema, next)
            }

          case Right(schemaList) =>
            current.toIntOption match {
              case Some(sectionInt) =>
                goDownSchema(schemaList(sectionInt), next)
              case None => Left(DecodingFailure(WrongTypeExpectation("Int", Json.fromString(current)), schema.hcursor))
            }
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

  private def parseConditionalValidationWithOneOfErrorMessage(
    schema: Json,
    json: Json,
    error: ValidationError
  ): String =
    error.schemaLocation match {
      case None => error.getMessage
      case Some(schemaLocation) =>
        val splitSchemaLocation: List[String] = schemaLocation.split("/").tail.toList
        val indexOfOneOf: Int = splitSchemaLocation.indexOf("oneOf")

        val maybeParsedOneOfOptions: Option[List[String]] =
          parseAllOneOfOptions(schema, splitSchemaLocation, indexOfOneOf)

        val errorProperty: String = splitSchemaLocation(indexOfOneOf - 1)

        val propertyNameInLocation = error.keyword === "pattern"
        val errorLocation: String = tryConvertErrorLocationToId(json, error.location, propertyNameInLocation)

        maybeParsedOneOfOptions match {
          case None => error.getMessage
          case Some(parsedOneOfOptions) =>
            val errorMessage: String =
              s"Property $errorProperty can only be used with ${parsedOneOfOptions.mkString(" OR ")}"
            constructCustomErrorMessage(errorLocation, errorMessage)
        }
    }

  private def parseAllOneOfOptions(schema: Json, splitSchemaLocation: List[String], indexOfOneOf: Int) =
    goDownSchema(schema, splitSchemaLocation.slice(0, indexOfOneOf + 1)) match {
      case Left(_) => None
      case Right(jsonValue) =>
        jsonValue.asArray match {
          case None => None
          case Some(oneOfOptions) =>
            getRequirementsForEveryOneOfOption(oneOfOptions) match {
              case Nil          => None
              case requirements => Some(requirements)
            }
        }
    }

  private def getRequirementsForEveryOneOfOption(oneOfOptions: Vector[Json]) =
    oneOfOptions.toList.flatMap { component =>
      component.findAllByKey("properties").headOption match {
        case None => None
        case Some(requiredProperties) =>
          requiredProperties.as[Map[String, Map[String, String]]] match {
            case Left(_) => None
            case Right(requiredPropertiesMap) =>
              Some(requiredPropertyValuesFromPattern(requiredPropertiesMap))
          }
      }
    }

  private def requiredPropertyValuesFromPattern(requiredPropertyAndPattern: Map[String, Map[String, String]]): String =
    requiredPropertyAndPattern.view
      .mapValues { requiredPattern =>
        requiredPattern.values
          .map { requiredValues =>
            requiredValues.substring(2, requiredValues.length - 2).replace("|", ", ")
          }
          .mkString(", ")
      }
      .map { case (requiredProperty, requiredValues) =>
        if (requiredValues.contains("?!")) {
          s"$requiredProperty not: [${requiredValues.substring(3, requiredValues.length - 3)}]"
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
    val allSameErrors: List[ValidationError] = errors.filter(_.location === error.location)
    if (allSameErrors.map(_.schemaLocation).exists(_.contains("#/$defs/stringOrEnCyObject"))) {
      parseCustomTypeError(allSameErrors, json, error, constructEnCyTypeError)

    } else if (!allSameErrors.map(_.schemaLocation.getOrElse("")).exists(_.contains("items/properties/"))) {
      val schemaLocationOrEmpty = allSameErrors.map(_.schemaLocation.getOrElse(""))

      if (schemaLocationOrEmpty.exists(_.contains("#/$defs/fields/properties/choices/"))) {
        parseCustomTypeError(allSameErrors, json, error, constructChoiceTypeError)
      } else if (schemaLocationOrEmpty.exists(_.contains("#/$defs/fields/properties/header/"))) {
        parseCustomTypeError(allSameErrors, json, error, constructHeaderTypeError)
      } else {
        parseNormalTypeError(schema, json, error, errors)
      }

    } else {
      parseNormalTypeError(schema, json, error, errors)
    }
  }

  private def parseCustomTypeError(
    allSameErrors: List[ValidationError],
    json: Json,
    error: ValidationError,
    customErrorConstructor: (Json, ValidationError) => String
  ) = {
    val maybeInvalidKeyMessage: Option[String] = keysFromErrorMessage(allSameErrors, "additionalProperties") match {
      case Nil         => None
      case invalidKeys => Some(s"Invalid key(s) [${invalidKeys.mkString(", ")}] are not permitted")
    }

    val maybeRequiredKeyMessage: Option[String] = keysFromErrorMessage(allSameErrors, "required") match {
      case Nil          => None
      case requiredKeys => Some(s"Missing key(s) [${requiredKeys.mkString(", ")}] are required")
    }

    List(Some(customErrorConstructor(json, error)), maybeRequiredKeyMessage, maybeInvalidKeyMessage).flatten
      .mkString(". ")
  }

  private def keysFromErrorMessage(allSameErrors: List[ValidationError], keyword: String): List[String] =
    allSameErrors
      .filter(_.keyword === keyword)
      .map(_.getMessage)
      .map("\\[.*]".r.findAllIn(_).next().drop(1).dropRight(1))

  private def constructEnCyTypeError(json: Json, error: ValidationError): String = {
    val (errorProperty, errorLocation) = getTypeErrorPropertyAndLocation(error, json)

    val errorMessage: String =
      s"Property $errorProperty expected type String or JSONObject with structure {en: String} or {en: String, cy: String}"
    constructCustomErrorMessage(errorLocation, errorMessage)
  }

  private def getTypeErrorPropertyAndLocation(error: ValidationError, json: Json) =
    error.location.split("/").last.toIntOption match {
      case Some(_) =>
        val property: String = error.location.split("/").dropRight(1).last
        val location: String =
          tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = false)

        (property, location)
      case None =>
        val property: String = error.location.split("/").last
        val location: String =
          tryConvertErrorLocationToId(json, error.location, propertyNameInLocation = true)

        (property, location)
    }

  private def constructChoiceTypeError(json: Json, error: ValidationError): String = {
    val (errorProperty, errorLocation) = getTypeErrorPropertyAndLocation(error, json)

    val errorMessage: String =
      s"Property $errorProperty expected type Array of either Strings or JSONObjects with required keys [en] and optional keys [cy, dynamic, value, hint, includeIf]"
    constructCustomErrorMessage(errorLocation, errorMessage)
  }

  private def constructHeaderTypeError(json: Json, error: ValidationError): String = {
    val (errorProperty, errorLocation) = getTypeErrorPropertyAndLocation(error, json)

    val errorMessage: String =
      s"Property $errorProperty expected type Array of either Strings or JSONObjects with structure {en: String} or {en: String, cy: String}"
    constructCustomErrorMessage(errorLocation, errorMessage)
  }

  private def parseNormalTypeError(
    schema: Json,
    json: Json,
    error: ValidationError,
    errors: NonEmptyList[ValidationError]
  ): String = {
    val maybeSchemaError: List[Option[String]] = errors
      .filter(_.location === error.location)
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
    val (errorProperty, errorLocation) = getTypeErrorPropertyAndLocation(error, json)

    errors.filter(_.location === error.location).filter(_.keyword === "type").map(_.getMessage) match {
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
              if (schemaLocation.split("/").last === "items" || schemaLocation === "#/$defs/fields") {
                s"Property $errorProperty expected type Array of [${requiredType.capitalize}], found Array of [$typeFound]"
              } else {
                s"Property $errorProperty expected type [${requiredType.capitalize}], found [$typeFound]"
              }
            constructCustomErrorMessage(errorLocation, errorMessage)
          case None => error.getMessage
        }
    }
}
