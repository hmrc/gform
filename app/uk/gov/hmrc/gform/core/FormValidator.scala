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

package uk.gov.hmrc.gform.core

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.libs.json._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._

object FormValidator {
  def conform(json: JsValue /* , schema: JsonSchema */ ): Opt[List[FormField]] = {
    /* for {
     *   res <- TemplateValidator.conform(schema, json).toEither
     * } yield res */

    val res = (json \ "fields").validate[List[FormField]]

    res match {
      case JsSuccess(success, _) => Right(success)
      case JsError(error) => Left(InvalidState(s"""|Error when reading 'FormField' class:
                                                   |Error: $error
                                                   |Input json: """.stripMargin + Json.prettyPrint(json)))
    }
  }

  def validate(formFields: List[FormField], section: Section): Opt[Unit] = {

    val ffSet = formFields.filterNot(_.value.isEmpty()).map(_.id).toSet

    val (templateFieldsMap, requiredFields) =
      section.atomicFields.foldLeft((Map.empty[FieldId, FieldValue], Set.empty[FieldId])) {
        case ((acc, reqAcc), fieldValue) =>

          fieldValue.`type` match {
            case Address(_) =>
              val res: Map[FieldId, FieldValue] = Address.fields(fieldValue.id).map(_ -> fieldValue).toMap
              val accRes = acc ++ res

              (accRes, reqAcc)

            case Date(_, _, _) =>
              val res: Map[FieldId, FieldValue] = Date.fields(fieldValue.id).map(_ -> fieldValue).toMap
              val accRes = acc ++ res

              (accRes, reqAcc)

            case Text(_, _) | Choice(_, _, _, _, _) | Group(_, _, _, _, _, _) | FileUpload() => // TODO - added Group just to compile; remove if possible
              val id = fieldValue.id
              val accRes = acc + (id -> fieldValue)

              val reqAccRes =
                fieldValue.mandatory match {
                  case true => reqAcc + id
                  case false => reqAcc
                }
              (accRes, reqAccRes)

            case InformationMessage(_, _) =>
              val id = fieldValue.id
              val accRes = acc + (id -> fieldValue)

              (accRes, reqAcc)
          }
      }

    val missingRequiredFields = (requiredFields diff ffSet)
    val requirementCheck: Opt[Unit] = if (missingRequiredFields.isEmpty) {
      Right(())
    } else {
      Left(InvalidState(s"Required fields ${missingRequiredFields.mkString(",")} are missing in form submission."))
    }

    val formFieldWithFieldValues: List[Opt[(FormField, FieldValue)]] =
      formFields.map { formField =>
        findTemplateFieldId(templateFieldsMap, formField.id) match {
          case Some(templateField) => Right((formField, templateField))
          case None => Left(InvalidState(s"Field ${formField.id} is not part of the template"))
        }
      }

    val formFieldWithFieldValuesU: Opt[List[(FormField, FieldValue)]] = formFieldWithFieldValues.sequenceU

    // TODO - All necessary validation of form fields based on their format

    for {
      _ <- requirementCheck
      _ <- formFieldWithFieldValuesU
    } yield ()
  }

  private def findTemplateFieldId(fieldMap: Map[FieldId, FieldValue], fieldId: FieldId) = {
    val repeatingGroupFieldId = raw"(.+)_\d+?".r

    val templateFieldId = fieldId.value match {
      case repeatingGroupFieldId(extractedFieldId) => FieldId(extractedFieldId)
      case _ => fieldId
    }

    fieldMap.get(templateFieldId)
  }
}
