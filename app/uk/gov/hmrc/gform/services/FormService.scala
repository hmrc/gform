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

import cats.data.EitherT
import cats.instances.future._
import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.gform.core._
import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.typeclasses._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

object FormService {

  def insertEmpty(userId: UserId, formTypeId: FormTypeId, envelopeId: EnvelopeId, formId: FormId)(implicit Insert: Insert[Form]): ServiceResponse[Form] = {
    val selector = Json.obj("_id" -> formId.value)
    val formData = FormData(userId, formTypeId, Version("0.3.0"), characterSet = "UTF-8", fields = Nil)
    val form = Form(formId, formData, envelopeId)
    fromFutureOptA(
      Insert(selector, form).map(_.right.map(_ => form))
    )
  }

  def updateFormData(formId: FormId, formData: FormData)(
    implicit
    FindOneForm: FindOne[Form],
    InsertForm: Insert[Form],
    UpdateForm: Update[Form]
  ): ServiceResponse[DbOperationResult] = {
    EitherT(

      FindOneForm(formSelector(formId))
        .map(_.get)
        .map(_.copy(formData = formData))
        .flatMap { f =>
          Logger.debug(Json.prettyPrint(Json.toJson(f)) + "FORMTH")
          UpdateForm(formSelector(formId), f)
        }
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

    val templateSelector =
      Json.obj(
        "formTypeId" -> formTypeId
      )

    // format: OFF
    for {
      _            <- operation match {
        case IsSave()   => success(())
        case IsUpdate() => fromFutureOptionA(FindOneForm(formSelector(form._id)))(InvalidState(s"Form ${form._id} not found")).map(_ => ())
      }
      formTemplate <- fromFutureOptionA(FindOneFormTemplate(templateSelector))(InvalidState(s"FormTemplate $templateSelector not found"))
      section      <- operation match {
        case IsTolerant() => success(Section("", None, None, None, None, None, None, List.empty[FieldValue])) // We are not using section in tolerant mode
        case IsStrict()   => fromOptA (TemplateValidator.getMatchingSection(formData.fields, formTemplate.sections))
      }
      _            <- operation match {
        case IsTolerant() => success(())
        case IsStrict()   => fromOptA(FormValidator.validate(formData.fields.toList, section))
      }
      dbResult     <- operation match {
        case IsSave()   => fromFutureOptA(InsertForm(formSelector(form._id), form))
        case IsUpdate() => fromFutureOptA(UpdateForm(formSelector(form._id), form))
      }
    } yield dbResult
    // format: ON
  }

  def allById(formTypeId: FormTypeId)(implicit FindForm: Find[Form]): ServiceResponse[List[Form]] = {

    val selector = Json.obj(
      "formTypeId" -> formTypeId.value
    )

    fromFutureA(FindForm(selector))
  }

  def getByTypeAndId(formTypeId: FormTypeId, formId: FormId)(implicit FindOneForm: FindOne[Form]): ServiceResponse[Form] = {

    val selector = Json.obj(
      "_id" -> formId.value,
      "formTypeId" -> formTypeId.value
    )

    fromFutureOptionA(FindOneForm(selector))(InvalidState(s"Form formTypeId ${formTypeId.value}, formId ${formId.value} not found"))
  }

  def getByIdAndVersion(formTypeId: FormTypeId, version: Version)(implicit FindForm: Find[Form]): ServiceResponse[List[Form]] = {

    val selector = Json.obj(
      "formTypeId" -> formTypeId.value,
      "version" -> version.value
    )

    fromFutureA(FindForm(selector))
  }

  def get(formId: FormId)(implicit FindOneForm: FindOne[Form], hc: HeaderCarrier): ServiceResponse[Form] =
    fromFutureOptionA(FindOneForm(formSelector(formId)))(InvalidState(s"Form _id ${formId.value}, not found"))

  def delete(formId: FormId)(implicit deleteForm: Delete[Form]) = {
    fromFutureOptA(deleteForm(formSelector(formId)))
  }

  private def formSelector(formId: FormId) =
    Json.obj("_id" -> formId.value)

}
