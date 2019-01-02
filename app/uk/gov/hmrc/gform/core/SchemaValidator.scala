/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.core

import cats.Monoid
import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.libs.json._
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.FormTemplateSchema

sealed trait JsonSchema {

  def conform[A](json: A)(implicit wrt: Writes[A]): ValidationResult = {
    def loop(schema: JsonSchema, json: JsValue): ValidationResult =
      schema match {
        case SObject(properties, required) =>
          val validationResults: Seq[ValidationResult] = properties.map {
            case Item(fieldName, SString) =>
              (json \ fieldName) match {
                case JsDefined(JsString(_))                           => Valid
                case JsUndefined() if (!required.contains(fieldName)) => Valid
                case otherwise =>
                  Invalid(s"FieldName '$fieldName' is missing or not a string, got: " + Json.prettyPrint(json))
              }
            case Item(fieldName, SBoolean) =>
              (json \ fieldName) match {
                case JsDefined(JsBoolean(_)) => Valid
                case otherwise =>
                  Invalid(s"FieldName '$fieldName' is missing or not a boolean, got: " + Json.prettyPrint(json))
              }
            case Item(fieldName, so @ SObject(_, _)) =>
              (json \ fieldName) match {
                case JsDefined(jo @ JsObject(contents)) => loop(so, jo)
                case otherwise =>
                  Invalid(s"FieldName '$fieldName' is missing or not an object, got: " + Json.prettyPrint(json))
              }
            case Item(fieldName, SArray(so)) =>
              (json \ fieldName) match {
                case JsDefined(JsArray(objs)) =>
                  val res = objs.map(obj => loop(so, obj)).toList
                  Monoid[ValidationResult].combineAll(res)
                case otherwise =>
                  Invalid(s"FieldName '$fieldName' is missing or not an array, got: " + Json.prettyPrint(json))
              }
          }
          Monoid[ValidationResult].combineAll(validationResults)
        case SString       => Invalid("No SString support on top level")
        case SBoolean      => Invalid("No SString support on top level")
        case SArray(items) => Invalid("No SArray support on top level")
      }
    loop(this, wrt.writes(json))
  }
}

case class SObject(properties: Seq[Item], required: Seq[String]) extends JsonSchema
case object SString extends JsonSchema
case object SBoolean extends JsonSchema
case class SArray(items: SObject) extends JsonSchema
case class Item(name: String, tpe: JsonSchema)

object SchemaValidator {

  def conform(schemaRep: FormTemplateSchema): Opt[JsonSchema] =
    //TODO verify if it works as is should work.
    //TODO maybe there exist ready to use solutions for json schema validation. Do research and use them instead of buggy this one
    loop(schemaRep.value)

  private def loop(json: JsValue): Opt[JsonSchema] =
    (json \ "type") match {
      case JsDefined(JsString(tpe)) =>
        tpe match {
          case "object"  => readObject(json)
          case "string"  => Right(SString)
          case "boolean" => Right(SBoolean)
          case "array"   => readArray(json)
          case otherwise => Left(UnexpectedState(s"Unsupported value for 'type' fieldName: '$otherwise'"))
        }
      case JsDefined(_) => Left(UnexpectedState(s"Expected 'type' to be one of 4 strings in json"))
      case otherwise    => Left(UnexpectedState("No 'type' fieldName found in json"))
    }

  private def readObject(json: JsValue): Opt[SObject] = {
    val properties = (json \ "properties") match {
      case JsDefined(JsObject(properties)) =>
        properties.toList.traverse[Opt, Item] {
          case (property, value) => loop(value).map(x => Item(property, x))
        }
      case otherwise => Left(UnexpectedState("No 'properties' fieldName of type object found in json"))
    }

    val required = (json \ "required") match {
      case JsDefined(JsArray(required)) =>
        required.toList.traverse[Opt, String] {
          case JsString(requiredFieldName) => Right(requiredFieldName)
          case nonString                   => Left(UnexpectedState("Required must be array with string"))
        }
      case otherwise => Left(UnexpectedState(s"No 'required' fieldName of type array found in json. Found: $otherwise"))
    }

    for {
      p <- properties
      r <- required
    } yield SObject(p, r)
  }

  private def readArray(json: JsValue): Opt[JsonSchema] =
    (json \ "items") match {
      case JsDefined(items @ JsObject(_)) => readObject(items).map(SArray(_))
      case otherwise                      => Left(UnexpectedState("No 'items' fieldName of type object found in json"))
    }
}
