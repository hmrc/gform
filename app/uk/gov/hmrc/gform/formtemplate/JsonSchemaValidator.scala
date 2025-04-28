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
import io.circe.{ Json, JsonObject }
import io.circe.jawn.JawnParser
import io.circe.schema.{ Schema, ValidationError }
import uk.gov.hmrc.gform.exceptions.SchemaValidationException

object JsonSchemaValidator {

  val parser = JawnParser(allowDuplicateKeys = false)

  def checkSchema(
    json: String,
    schema: String,
    errorParser: (NonEmptyList[ValidationError], Json, Json) => SchemaValidationException
  ): Either[SchemaValidationException, Unit] =
    parser.parse(json) match {
      case Right(originalJson) =>
        val strippedJson = removeUnderscoreKeysAndCommentsPreservingTopId(originalJson)
        validateJson(strippedJson, originalJson, schema, errorParser)

      case Left(parsingFailure) =>
        Left(SchemaValidationException("Json error: " + parsingFailure))
    }

  private def validateJson(
    filteredJson: Json,
    originalJson: Json,
    schema: String,
    errorParser: (NonEmptyList[ValidationError], Json, Json) => SchemaValidationException
  ): Either[SchemaValidationException, Unit] =
    parser.parse(schema) match {
      case Left(parsingFailure) =>
        Left(SchemaValidationException("Schema error: " + parsingFailure))
      case Right(schemaJson) =>
        val formTemplateSchema: Schema = Schema.load(schemaJson)
        val validated: ValidatedNel[ValidationError, Unit] = formTemplateSchema.validate(filteredJson)

        validated.leftMap { errors =>
          errorParser(errors, schemaJson, filteredJson)
        }.toEither
    }

  private def removeUnderscoreKeysAndCommentsPreservingTopId(json: Json): Json =
    json.arrayOrObject(
      json,
      arr => Json.fromValues(arr.map(removeUnderscoreKeysAndCommentsPreservingTopId)),
      obj =>
        Json.fromJsonObject(
          JsonObject.fromIterable(
            obj.toIterable
              .collect {
                case ("_id", value) => "_id" -> value //keep _id at top level (form ID)
                case (key, value) if !key.startsWith("_") && key != "comment" =>
                  key -> value
              }
              .map { case (k, v) =>
                val cleanedValue = if (v.isObject || v.isArray) removeUnderscoreKeysAndCommentsPreservingTopId(v) else v
                k -> cleanedValue
              }
          )
        )
    )
}
