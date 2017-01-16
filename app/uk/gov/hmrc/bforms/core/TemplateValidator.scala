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

import cats.implicits._
import cats.Monoid
import play.api.libs.json._
import uk.gov.hmrc.bforms.exceptions.{ InvalidState, UnexpectedState }

sealed trait ValidationResult {
  def toEither: Opt[Unit] = this match {
    case Valid => Right(())
    case Invalid(reason) => Left(InvalidState(reason))
  }
}

case object Valid extends ValidationResult
case class Invalid(reason: String) extends ValidationResult

case class TemplateField(id: String, mandatory: String, format: Option[String])

object TemplateField {
  implicit val reads: Reads[TemplateField] = Json.reads[TemplateField]
}

object ValidationResult {

  implicit val validationResultMonoid = new Monoid[ValidationResult] {
    def empty: ValidationResult = Valid
    def combine(x: ValidationResult, y: ValidationResult): ValidationResult = (x, y) match {
      case (Valid, Valid) => Valid
      case (i @ Invalid(_), _) => i
      case (_, i @ Invalid(_)) => i
    }
  }
}

object TemplateValidator {

  def conform(schema: JsonSchema, json: JsValue): ValidationResult = {
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
            case JsDefined(jo @ JsObject(contents)) => conform(so, jo)
            case otherwise => Invalid(s"FieldName '$fieldName' is missing or not an object, got: " + Json.prettyPrint(json))
          }
          case Item(fieldName, SArray(so)) => (json \ fieldName) match {
            case JsDefined(JsArray(objs)) =>
              val res = objs.map(obj => conform(so, obj)).toList
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

  def extractFields(json: JsValue): Opt[Seq[TemplateField]] = {
    val templateFieldsRaw: Seq[JsResult[List[TemplateField]]] = (json \\ "fields").map(_.validate[List[TemplateField]])

    val templateFields: List[Opt[Seq[TemplateField]]] = templateFieldsRaw.map {
      case JsSuccess(success, _) => Right(success)
      case JsError(error) => Left(InvalidState(s"""|Error when reading 'TemplateField' class:
                                                   |Error: $error
                                                   |Input json: """.stripMargin + Json.prettyPrint(json)))
    }.toList
    templateFields.sequenceU.map(_.flatten)
  }
}
