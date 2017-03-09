/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.core

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.Monoid
import play.api.libs.json._
import uk.gov.hmrc.bforms.exceptions.{ InvalidState, InvalidStateWithJson }
import uk.gov.hmrc.bforms.model.Schema

sealed trait JsonSchema {

  def conform[A](json: A)(implicit wrt: Writes[A]): ValidationResult = {
    def loop(schema: JsonSchema, json: JsValue): ValidationResult = {
      schema match {
        case SObject(properties, required) =>
          val validationResults: Seq[ValidationResult] = properties.map {
            case Item(fieldName, SString) => (json \ fieldName) match {
              case JsDefined(JsString(_)) => Valid
              case otherwise => Invalid(s"FieldName '$fieldName' is missing or not a string, got: " + Json.prettyPrint(json))
            }
            case Item(fieldName, SBoolean) => (json \ fieldName) match {
              case JsDefined(JsBoolean(_)) => Valid
              case otherwise => Invalid(s"FieldName '$fieldName' is missing or not a boolean, got: " + Json.prettyPrint(json))
            }
            case Item(fieldName, so @ SObject(_, _)) => (json \ fieldName) match {
              case JsDefined(jo @ JsObject(contents)) => loop(so, jo)
              case otherwise => Invalid(s"FieldName '$fieldName' is missing or not an object, got: " + Json.prettyPrint(json))
            }
            case Item(fieldName, SArray(so)) => (json \ fieldName) match {
              case JsDefined(JsArray(objs)) =>
                val res = objs.map(obj => loop(so, obj)).toList
                Monoid[ValidationResult].combineAll(res)
              case otherwise => Invalid(s"FieldName '$fieldName' is missing or not an array, got: " + Json.prettyPrint(json))
            }
          }
          Monoid[ValidationResult].combineAll(validationResults)
        case SString => Invalid("No SString support on top level")
        case SBoolean => Invalid("No SString support on top level")
        case SArray(items) => Invalid("No SArray support on top level")
      }
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
  def conform(schemaRep: Schema): Opt[JsonSchema] = {
    loop(schemaRep.value)
  }

  private def loop(json: JsValue): Opt[JsonSchema] = {
    (json \ "type") match {
      case JsDefined(JsString(tpe)) => tpe match {
        case "object" => readObject(json)
        case "string" => Right(SString)
        case "boolean" => Right(SBoolean)
        case "array" => readArray(json)
        case otherwise => Left(InvalidState(s"Unsupported value for 'type' fieldName: '$otherwise'"))
      }
      case otherwise => Left(InvalidStateWithJson("No 'type' fieldName of type string found in json", json))
    }
  }

  private def readObject(json: JsValue): Opt[SObject] = {
    val properties = (json \ "properties") match {
      case JsDefined(JsObject(properties)) =>
        val res = properties.map { case (property, value) => loop(value).map(x => Item(property, x)) }.toList
        res.sequenceU
      case otherwise => Left(InvalidStateWithJson("No 'properties' fieldName of type object found in json", json))
    }

    val required = (json \ "required") match {
      case JsDefined(JsArray(required)) =>
        val res: List[Opt[String]] = required.map {
          case JsString(requiredFieldName) => Right(requiredFieldName)
          case nonString => Left(InvalidState("Required must be array with string"))
        }.toList
        res.sequenceU
      case otherwise => Left(InvalidStateWithJson("No 'required' fieldName of type array found in json", json))
    }

    for {
      p <- properties
      r <- required
    } yield SObject(p, r)
  }

  private def readArray(json: JsValue): Opt[JsonSchema] = {
    (json \ "items") match {
      case JsDefined(items @ JsObject(_)) => readObject(items).map(SArray(_))
      case otherwise => Left(InvalidStateWithJson("No 'items' fieldName of type object found in json", json))
    }
  }
}
