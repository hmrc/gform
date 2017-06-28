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

package uk.gov.hmrc.gform.services

import java.util.UUID

import cats.instances.future._
import play.api.libs.json.Json
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses.{ Find, FindOne, Insert, Update }

import scala.concurrent.ExecutionContext.Implicits.global

object FormService {

  def insertEmpty(formTypeId: FormTypeId, version: String)(implicit Insert: Insert[Form]): ServiceResponse[Form] = {
    val formId = FormId(UUID.randomUUID().toString)
    val selector = Json.obj("_id" -> formId.value)
    val formData = FormData(formTypeId, version, characterSet = "UTF-8", fields = Nil)
    val form = Form(formId, formData)
    fromFutureOptA(
      Insert(selector, form).map(_.right.map(_ => form))
    )
  }

  def saveOrUpdate(
    form: Form,
    operation: MongoOperation
  )(
    implicit
    FindOneFormTemplate: FindOne[FormTemplate],
    FindOneForm: FindOne[Form],
    InsertForm: Insert[Form],
    UpdateForm: Update[Form]
  ): ServiceResponse[DbOperationResult] = {
    val formData = form.formData
    val formTypeId = formData.formTypeId
    val version = formData.version

    val templateSelector = Json.obj(
      "formTypeId" -> formTypeId,
      "version" -> version
    )

    val formSelector = Json.obj("_id" -> form._id)

    // format: OFF
    for {
      _            <- operation match {
        case IsSave()   => success(())
        case IsUpdate() => fromFutureOptionA(FindOneForm(formSelector))(InvalidState(s"Form $formSelector not found")).map(_ => ())
      }
      formTemplate <- fromFutureOptionA(FindOneFormTemplate(templateSelector))(InvalidState(s"FormTemplate $templateSelector not found"))
      section      <- operation match {
        case IsTolerant() => success(Section("", None, None, List.empty[FieldValue])) // We are not using section in tolerant mode
        case IsStrict()   => fromOptA (TemplateValidator.getMatchingSection(formData.fields, formTemplate.sections))
      }
      _            <- operation match {
        case IsTolerant() => success(())
        case IsStrict()   => fromOptA(FormValidator.validate(formData.fields.toList, section))
      }
      dbResult     <- operation match {
        case IsSave()   => fromFutureOptA(InsertForm(formSelector, form))
        case IsUpdate() => fromFutureOptA(UpdateForm(formSelector, form))
      }
    } yield dbResult
    // format: ON
  }

  def allById(formTypeId: FormTypeId)(implicit FindForm: Find[Form]): ServiceResponse[List[Form]] = {

    val selector = Json.obj(
      "formTypeId" -> formTypeId
    )

    fromFutureA(FindForm(selector))
  }

  def getByTypeAndId(formTypeId: FormTypeId, formId: FormId)(implicit FindOneForm: FindOne[Form]): ServiceResponse[Form] = {

    val selector = Json.obj(
      "_id" -> formId,
      "formTypeId" -> formTypeId
    )

    fromFutureOptionA(FindOneForm(selector))(InvalidState(s"Form formTypeId $formTypeId, formId $formId not found"))
  }

  def getByIdAndVersion(formTypeId: FormTypeId, version: String)(implicit FindForm: Find[Form]): ServiceResponse[List[Form]] = {

    val selector = Json.obj(
      "formTypeId" -> formTypeId,
      "version" -> version
    )

    fromFutureA(FindForm(selector))
  }

  def get(formTypeId: FormTypeId, version: String, formId: FormId)(implicit FindOneForm: FindOne[Form]): ServiceResponse[Form] = {

    val selector = Json.obj(
      "_id" -> formId,
      "version" -> version,
      "formTypeId" -> formTypeId
    )

    fromFutureOptionA(FindOneForm(selector))(InvalidState(s"Form _id $formId, version: $version, formTypeId: $formTypeId not found"))
  }
}
