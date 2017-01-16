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
import play.api.libs.json._
import uk.gov.hmrc.bforms.exceptions.{ InvalidState, UnexpectedState }

sealed trait JsonSchema
case class SObject(properties: Seq[Item], required: Seq[String]) extends JsonSchema
case object SString extends JsonSchema
case object SBoolean extends JsonSchema
case class SArray(items: SObject) extends JsonSchema
case class Item(name: String, tpe: JsonSchema)

object SchemaValidator {
  def conform(json: JsValue): Opt[JsonSchema] = {
    (json \ "type") match {
      case JsDefined(JsString(tpe)) => tpe match {
        case "object" => readObject(json)
        case "string" => Right(SString)
        case "boolean" => Right(SBoolean)
        case "array" => readArray(json)
        case otherwise => Left(InvalidState(s"Unsupported value for type: '$otherwise'"))
      }
      case otherwise => Left(InvalidState("No 'type' fieldName of type string found in json: " + Json.prettyPrint(json)))
    }
  }

  def readObject(json: JsValue): Opt[SObject] = {
    val properties = (json \ "properties") match {
      case JsDefined(JsObject(properties)) =>
        val res = properties.map { case (property, value) => conform(value).map(x => Item(property, x)) }.toList
        res.sequenceU
      case otherwise => Left(InvalidState("No 'properties' fieldName of type object found in json: " + Json.prettyPrint(json)))
    }

    val required = (json \ "required") match {
      case JsDefined(JsArray(required)) =>
        val res: List[Opt[String]] = required.map {
          case JsString(requiredFieldName) => Right(requiredFieldName)
          case nonString => Left(InvalidState("Required must be array with string"))
        }.toList
        res.sequenceU
      case otherwise => Left(InvalidState("No 'required' fieldName of type array found in json: " + Json.prettyPrint(json)))
    }

    for {
      p <- properties
      r <- required
    } yield SObject(p, r)
  }

  def readArray(json: JsValue): Opt[JsonSchema] = {
    (json \ "items") match {
      case JsDefined(items @ JsObject(_)) => readObject(items).map(SArray(_))
      case otherwise => Left(InvalidState("No 'items' fieldName of type object found in json: " + Json.prettyPrint(json)))
    }
  }
}
