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
import io.circe.{ Decoder, DecodingFailure, HCursor, Json }
import io.circe.schema.ValidationError
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

import scala.annotation.tailrec
import scala.util.matching.Regex

object JsonSchemaErrorParser {

  private val errorMessagePattern: Regex = """.*"errorMessage":"(.*?)".*""".r
  private val authModuleValues =
    List("hmrc", "email", "anonymous", "hmrcAny", "hmrcVerified", "composite")
  private val acceptedAuthModules = authModuleValues.mkString(", ")
  private val authConfigPath = "#/authConfig"

  private val allowedAuthPropertyValues = Map(
    "agentAccess"    -> List("requireMTDAgentEnrolment", "denyAnyAgentAffinityUser", "allowAnyAgentAffinityUser"),
    "enrolmentCheck" -> List("never", "always", "forNonAgents"),
    "minimumCL"      -> List("200", "250"),
    "emailService"   -> List("dc", "notify")
  )

  private val expectedAuthPropertyTypes = Map(
    "serviceId"                 -> "a string",
    "regimeId"                  -> "a string",
    "legacyFcEnrolmentVerifier" -> "a string",
    "enrolmentOutcomes"         -> "an object containing all five outcome pages",
    "enrolmentSection"          -> "an enrolment section object",
    "ivFailure"                 -> "a string",
    "minimumCL"                 -> "the string '200' or '250'",
    "allowOrganisations"        -> "a boolean",
    "allowSAIndividuals"        -> "a boolean",
    "emailCodeTemplate"         -> "a string",
    "emailService"              -> "the string 'dc' or 'notify'",
    "emailUseInfo"              -> "a string",
    "emailCodeHelp"             -> "a string",
    "emailConfirmation"         -> "a string",
    "configs"                   -> "a non-empty array of hmrc or email auth configs"
  )

  private case class AuthConfigAt(path: String, value: Json)

  def parseErrorMessages(errors: NonEmptyList[ValidationError], schema: Json, json: Json): SchemaValidationException = {
    val authConfigMessage = parseAuthConfigError(errors, json)
    val errorsToParse = authConfigMessage match {
      case Some(_) => errors.toList.filterNot(isAuthConfigError)
      case None    => errors.toList
    }
    val parsedMessages = errorsToParse.map(parseError(_, errors, schema, json))

    val messages = (authConfigMessage.toList ++ parsedMessages).distinct
    SchemaValidationException(NonEmptyList.fromList(messages).getOrElse(NonEmptyList.one("")))
  }

  private def parseError(
    error: ValidationError,
    errors: NonEmptyList[ValidationError],
    schema: Json,
    json: Json
  ): String = {
    val schemaLocation = error.schemaLocation.getOrElse("")
    lazy val hasTypeErrorAtSameLocation = errors.exists(e => e.location === error.location && e.keyword === "type")

    error.keyword match {
      case _ if schemaLocation.contains("dependencies") && schemaLocation.contains("oneOf") =>
        parseConditionalValidationWithOneOfErrorMessage(schema, json, error)
      case _ if schemaLocation.contains("dependencies") =>
        parseConditionalValidationErrorMessage(schema, json, error)
      case _ if errorMessagePattern.findFirstIn(error.getMessage).isDefined => parseErrorMessage(error)
      case _ if hasTypeErrorAtSameLocation                                  => parseTypeError(error, errors, json, schema)
      case "required"                                                       => parseRequiredError(schema, json, error)
      case "pattern"                                                        => parsePatternError(schema, json, error)
      case "additionalProperties"                                           => parseAdditionalPropertiesError(errors, json, error)
      case _                                                                => error.getMessage
    }
  }

  private def isAuthConfigError(error: ValidationError): Boolean =
    error.location == authConfigPath || error.location.startsWith(authConfigPath + "/")

  private def parseAuthConfigError(errors: NonEmptyList[ValidationError], json: Json): Option[String] = {
    val authConfigs =
      json.hcursor.downField("authConfig").focus.toList.flatMap(collectAuthConfigs(_, authConfigPath))
    val configsWithErrors = authConfigs
      .filter(config =>
        errors.exists(error => error.location == config.path || error.location.startsWith(config.path + "/"))
      )
      .sortBy(config => -config.path.count(_ == '/'))

    configsWithErrors.iterator
      .map(config => authConfigErrorFor(config, errors))
      .collectFirst { case Some(message) => message }
  }

  private def collectAuthConfigs(value: Json, path: String): List[AuthConfigAt] =
    value.asObject.toList.flatMap { obj =>
      val current = AuthConfigAt(path, value)
      val nested = obj("configs").flatMap(_.asArray).toList.flatten.zipWithIndex.flatMap { case (config, index) =>
        collectAuthConfigs(config, s"$path/configs/$index")
      }
      current :: nested
    }

  private def authConfigErrorFor(
    config: AuthConfigAt,
    errors: NonEmptyList[ValidationError]
  ): Option[String] = {
    val fields = config.value.asObject.map(_.toMap).getOrElse(Map.empty)
    val module = fields.get("authModule").flatMap(_.asString)

    module match {
      case Some(authModule) if authModuleValues.contains(authModule) =>
        val unknownProperties = fields.keySet -- (authProperties(authModule).toSet + "authModule")
        val hasMissingProperties = requiredAuthProperties(authModule, fields).required.exists(!fields.contains(_))

        Option
          .when(unknownProperties.nonEmpty)(authPropertiesMessage(authModule, unknownProperties))
          .orElse(invalidAuthPropertyValue(config, authModule, fields, errors))
          .orElse(invalidAuthPropertyType(config, authModule, errors))
          .orElse(Option.when(hasMissingProperties)(authPropertiesMessage(authModule, unknownProperties, fields)))
          .map(authConfigErrorMessage(config, _))
      case None if !fields.contains("authModule") =>
        Some(
          authConfigErrorMessage(
            config,
            s"authConfig requires property [authModule] to be present. Accepted authModule values are [$acceptedAuthModules]."
          )
        )
      case _ =>
        Some(
          authConfigErrorMessage(
            config,
            s"authModule: '${module.getOrElse("")}' is Invalid. Accepted authModule values are [$acceptedAuthModules]."
          )
        )
    }
  }

  private def authConfigErrorMessage(config: AuthConfigAt, message: String): String =
    constructCustomErrorMessage(config.path, message)

  private val hmrcProperties = List(
    "agentAccess",
    "enrolmentCheck",
    "serviceId",
    "regimeId",
    "legacyFcEnrolmentVerifier",
    "enrolmentOutcomes",
    "enrolmentSection"
  )
  private val emailProperties =
    List("emailCodeTemplate", "emailService", "emailUseInfo", "emailCodeHelp", "emailConfirmation")
  private val hmrcVerifiedProperties =
    List("ivFailure", "agentAccess", "minimumCL", "allowOrganisations", "allowSAIndividuals")

  private def authProperties(authModule: String): List[String] =
    authModule match {
      case "hmrc"         => hmrcProperties
      case "email"        => emailProperties
      case "hmrcVerified" => hmrcVerifiedProperties
      case "composite"    => List("configs")
      case _              => Nil
    }

  // `dependants` are the present properties that cause `required` to be required
  private case class RequiredAuthProperties(required: List[String], dependants: List[String] = Nil)

  private val propertiesRequiringServiceId =
    List("enrolmentCheck", "enrolmentOutcomes", "enrolmentSection", "regimeId", "legacyFcEnrolmentVerifier")

  private def requiredAuthProperties(authModule: String, fields: Map[String, Json]): RequiredAuthProperties =
    authModule match {
      case "hmrc" if fields.contains("serviceId") => RequiredAuthProperties(List("enrolmentOutcomes"))
      case "hmrc" =>
        propertiesRequiringServiceId.filter(fields.contains) match {
          case Nil        => RequiredAuthProperties(Nil)
          case dependants => RequiredAuthProperties(List("serviceId"), dependants)
        }
      case "email"        => RequiredAuthProperties(List("emailCodeTemplate"))
      case "hmrcVerified" => RequiredAuthProperties(hmrcVerifiedProperties)
      case "composite"    => RequiredAuthProperties(List("configs"))
      case _              => RequiredAuthProperties(Nil)
    }

  // Only hmrc and email have optional properties; hmrc's required ones depend on serviceId so all are listed
  private def optionalAuthProperties(authModule: String): List[String] =
    authModule match {
      case "hmrc"  => hmrcProperties
      case "email" => emailProperties.filterNot(_ == "emailCodeTemplate")
      case _       => Nil
    }

  private def authPropertiesMessage(
    authModule: String,
    unknownProperties: Set[String],
    fields: Map[String, Json] = Map.empty
  ): String = {
    val requirements = requiredAuthProperties(authModule, fields)
    val optional = optionalAuthProperties(authModule)
    val missing = requirements.required.filterNot(fields.contains)

    val summary = authPropertiesSummary(authModule, unknownProperties, missing)
    val requiredMessage = requirements match {
      case RequiredAuthProperties(required, dependants) if dependants.nonEmpty =>
        Some(s" Properties [${dependants.mkString(", ")}] requires [${required.mkString(", ")}].")
      case RequiredAuthProperties(required, _) if required.nonEmpty =>
        Some(s" Required properties: [${required.mkString(", ")}].")
      case _ => None
    }
    val optionalMessage = Option.when(optional.nonEmpty)(s" Optional properties: [${optional.mkString(", ")}].")
    val hmrcServiceIdMessage =
      Option.when(authModule == "hmrc")(" When serviceId is provided, enrolmentOutcomes is also required.")

    (summary :: List(requiredMessage, optionalMessage, hmrcServiceIdMessage).flatten).mkString
  }

  private def authPropertiesSummary(authModule: String, unknown: Set[String], missing: List[String]): String =
    (authProperties(authModule), unknown, missing) match {
      case (Nil, _, _) => s"authModule: '$authModule' does not accepts any other properties."
      case (_, unknown, _) if unknown.nonEmpty =>
        s"authModule: '$authModule' does not accept [${unknown.mkString(", ")}]."
      case (_, _, missing) if missing.nonEmpty => s"authModule: '$authModule' is missing all required properties."
      case _                                   => ""
    }

  private def invalidAuthPropertyValue(
    config: AuthConfigAt,
    authModule: String,
    fields: Map[String, Json],
    errors: NonEmptyList[ValidationError]
  ): Option[String] = {
    val enumErrorProperties = errors.toList.filter(_.keyword == "enum").flatMap(propertyOf(_, config))

    (enumErrorProperties ++ allowedAuthPropertyValues.keys).distinct.iterator
      .flatMap { property =>
        for {
          accepted <- allowedAuthPropertyValues.get(property)
          value    <- fields.get(property)
          if !value.asString.exists(accepted.contains)
        } yield s"Property '$property' for authModule: '$authModule' expected one of [${accepted.mkString(", ")}]."
      }
      .nextOption()
  }

  private def invalidAuthPropertyType(
    config: AuthConfigAt,
    authModule: String,
    errors: NonEmptyList[ValidationError]
  ): Option[String] =
    errors.toList.iterator
      .flatMap { error =>
        for {
          property <- propertyOf(error, config)
          expected <- expectedAuthPropertyTypes.get(property)
        } yield s"Property '$property' for authModule '$authModule' expected $expected."
      }
      .nextOption()

  // The direct property of the auth config the error belongs to, if any
  private def propertyOf(error: ValidationError, config: AuthConfigAt): Option[String] =
    Option
      .when(error.location.startsWith(config.path + "/"))(error.location.stripPrefix(config.path + "/"))
      .flatMap(_.split("/").headOption)

  private def parseErrorMessage(error: ValidationError): String =
    error.getMessage match {
      case errorMessagePattern(msg) =>
        constructCustomErrorMessage(error.location, msg)
      case _ => error.getMessage
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
      case "pattern" | "enum" =>
        parsePatternEnumConditionalValidationError(schema, json, error)

      case "required" =>
        parseRequiredConditionalValidationError(schema, json, error)

      case _ => error.getMessage
    }

  private def parsePatternEnumConditionalValidationError(schema: Json, json: Json, error: ValidationError): String = {
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

  trait JsonPropertyValue

  case class JsonString(value: String) extends JsonPropertyValue
  case class JsonBoolean(values: List[Boolean]) extends JsonPropertyValue

  object JsonPropertyValue {

    implicit val stringValueDecoder: Decoder[JsonString] = new Decoder[JsonString] {
      final def apply(c: HCursor): Decoder.Result[JsonString] =
        for {
          value <- c.as[String]
        } yield JsonString(value)
    }

    implicit val booleanValueDecoder: Decoder[JsonBoolean] = new Decoder[JsonBoolean] {
      final def apply(c: HCursor): Decoder.Result[JsonBoolean] =
        for {
          values <- c.as[List[Boolean]]
        } yield JsonBoolean(values)
    }

    implicit val propertyValueDecoder: Decoder[JsonPropertyValue] = new Decoder[JsonPropertyValue] {
      final def apply(c: HCursor): Decoder.Result[JsonPropertyValue] =
        c.as[String]
          .map(JsonString.apply)
          .orElse(
            c.as[List[Boolean]].map(JsonBoolean.apply)
          )
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
        requirements.as[Map[String, Map[String, JsonPropertyValue]]] match {
          case Left(_) => error.getMessage
          case Right(propertyAndPattern) =>
            val errorMessage: String =
              s"Property $property can only be used with ${requiredPropertyValuesFromPatternEnum(propertyAndPattern)}"
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
          requiredProperties.as[Map[String, Map[String, JsonPropertyValue]]] match {
            case Left(_) => None
            case Right(requiredPropertiesMap) =>
              Some(requiredPropertyValuesFromPatternEnum(requiredPropertiesMap))
          }
      }
    }

  private def requiredPropertyValuesFromPatternEnum(
    requiredPropertyAndPattern: Map[String, Map[String, JsonPropertyValue]]
  ): String =
    requiredPropertyAndPattern.view
      .mapValues { requiredPattern =>
        requiredPattern.values
          .map {
            case JsonString(value)   => value.substring(2, value.length - 2).replace("|", ", ")
            case JsonBoolean(values) => values.map(_.toString).mkString(", ")
            case other               => throw new IllegalArgumentException(s"Unsupported type: $other")
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
    val allLocations = allSameErrors.map(_.schemaLocation)
    if (allLocations.exists(_.contains("#/$defs/stringOrEnCyObject"))) {
      parseCustomTypeError(allSameErrors, json, error, constructEnCyTypeError)
    } else if (allLocations.exists(_.contains("#/$defs/fields/properties/displayInSummary"))) {
      parseCustomTypeError(allSameErrors, json, error, constructDisplayInSummaryError)
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

  private def constructDisplayInSummaryError(json: Json, error: ValidationError): String = {
    val (errorProperty, errorLocation) = getTypeErrorPropertyAndLocation(error, json)

    val errorMessage: String =
      s"Property $errorProperty expected type Boolean or String"
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

      case Right(requiredTypeJson) =>
        val maybeRequiredType: Option[String] = if (requiredTypeJson.isArray) {
          requiredTypeJson.as[List[String]].map(_.mkString(", ")).toOption
        } else if (requiredTypeJson.isString) {
          requiredTypeJson.asString
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
