/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import play.api.libs.json.Json
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.formtemplate.{ HandlebarsSchemaErrorParser, JsonSchemaValidator }
import uk.gov.hmrc.gform.sharedmodel.HandlebarsSchemaId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

object JsonSchemaValidationSupport {

  final case class SchemaValidationConfig(
    destinationId: DestinationId,
    destinationType: String,
    validateHandlebarPayload: Boolean,
    jsonSchemaName: Option[String],
    payloadType: TemplateType,
    payload: Option[String],
    jsonSchema: Option[JsValue]
  )

  def schemaValidationConfig(destination: Destination): Option[SchemaValidationConfig] =
    destination match {
      case d: Destination.DataStore =>
        Some(
          SchemaValidationConfig(
            destinationId = d.id,
            destinationType = Destination.dataStore,
            validateHandlebarPayload = d.validateHandlebarPayload,
            jsonSchemaName = d.jsonSchemaName,
            payloadType = TemplateType.JSON,
            payload = d.payload,
            jsonSchema = d.jsonSchema
          )
        )
      case d: Destination.HandlebarsHttpApi =>
        Some(
          SchemaValidationConfig(
            destinationId = d.id,
            destinationType = Destination.handlebarsHttpApi,
            validateHandlebarPayload = d.validateHandlebarPayload,
            jsonSchemaName = d.jsonSchemaName,
            payloadType = d.payloadType,
            payload = d.payload,
            jsonSchema = d.jsonSchema
          )
        )
      case d: Destination.AsyncHandlebarsHttpApi =>
        Some(
          SchemaValidationConfig(
            destinationId = d.id,
            destinationType = Destination.asyncHandlebarsHttpApi,
            validateHandlebarPayload = d.validateHandlebarPayload,
            jsonSchemaName = d.jsonSchemaName,
            payloadType = d.payloadType,
            payload = d.payload,
            jsonSchema = d.jsonSchema
          )
        )
      case _ => None
    }

  def resolvedSchemaName(defaultName: String, jsonSchemaName: Option[String]): String =
    jsonSchemaName.map(_.trim).filter(_.nonEmpty).getOrElse(defaultName)

  def resolvedSchemaId(formTemplateId: FormTemplateId, destination: Destination): Option[HandlebarsSchemaId] =
    schemaValidationConfig(destination)
      .filter(_.validateHandlebarPayload)
      .map(cfg => HandlebarsSchemaId(resolvedSchemaName(formTemplateId.value, cfg.jsonSchemaName)))

  def withResolvedSchema(destination: Destination, schema: JsValue): Destination =
    destination match {
      case d: Destination.DataStore              => d.copy(jsonSchema = Some(schema))
      case d: Destination.HandlebarsHttpApi      => d.copy(jsonSchema = Some(schema))
      case d: Destination.AsyncHandlebarsHttpApi => d.copy(jsonSchema = Some(schema))
      case other                                 => other
    }

  def validatePayload(destination: Destination, payload: String): Either[String, Unit] =
    schemaValidationConfig(destination) match {
      case Some(config) if config.validateHandlebarPayload =>
        val schemaName = config.jsonSchemaName.map(_.trim).filter(_.nonEmpty).getOrElse("<formTemplateId>")

        if (config.payloadType != TemplateType.JSON) {
          Left(
            s"JSON schema validation is not supported for payloadType '${config.payloadType.toString}'. Destination '${config.destinationId.id}' requires payloadType 'JSON'. Schema: '$schemaName'."
          )
        } else {
          config.jsonSchema match {
            case Some(schema) =>
              JsonSchemaValidator.checkSchema(
                payload,
                schema.toString,
                HandlebarsSchemaErrorParser.parseErrorMessages
              ) match {
                case Left(validationEx) =>
                  val errors = Json.prettyPrint(validationEx.errors)
                  Left(s"JSON schema validation is failed for schema '$schemaName'. JSON validation errors: $errors")
                case Right(value) => Right(value)
              }
            case None =>
              Left(s"JSON schema '$schemaName' does not exist for the destination '${config.destinationId.id}'")
          }
        }
      case _ => Right(())
    }
}
