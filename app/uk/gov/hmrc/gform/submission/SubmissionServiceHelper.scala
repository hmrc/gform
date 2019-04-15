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

package uk.gov.hmrc.gform.submission

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.traverse._
import play.api.libs.json.Json
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.{ RepeatingComponentService, SectionHelper }
import uk.gov.hmrc.gform.sharedmodel.Visibility
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, SubmissionData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object SubmissionServiceHelper {

  def getSectionFormFields(
    form: Form,
    formTemplate: FormTemplate,
    affinityGroup: Option[AffinityGroup]): Opt[List[SectionFormField]] = {

    val data: Map[FormComponentId, FormField] = form.formData.fields.map(field => field.id -> field).toMap

    val formFieldByFieldValue: FormComponent => Opt[(List[FormField], FormComponent)] = fieldValue => {
      val fieldValueIds: List[FormComponentId] =
        fieldValue.`type` match {
          case Address(_)    => Address.fields(fieldValue.id).toList
          case Date(_, _, _) => Date.fields(fieldValue.id).toList
          case FileUpload()  => List(fieldValue.id)
          case UkSortCode(_) => UkSortCode.fields(fieldValue.id).toList
          case Text(_, _, _, _) | TextArea(_, _, _) | Choice(_, _, _, _, _) | Group(_, _, _, _, _, _) =>
            List(fieldValue.id)
          case InformationMessage(_, _) => Nil
          case HmrcTaxPeriod(_, _, _)   => List(fieldValue.id)
          case _                        => Nil
        }

      val formFieldAndFieldValues: Opt[List[FormField]] = {
        fieldValueIds
          .map { fieldValueId =>
            data.get(fieldValueId) match {
              case Some(formField) => Right(formField)
              case None            => Left(UnexpectedState(s"No formField for field.id: ${fieldValue.id} found"))
            }
          }
          .partition(_.isLeft) match {
          case (Nil, list) => Right(for (Right(formField) <- list) yield formField)
          case (invalidStates, _) =>
            Left(UnexpectedState((for (Left(invalidState) <- invalidStates) yield invalidState.error).mkString(", ")))
        }
      }

      formFieldAndFieldValues match {
        case Right(list)        => Right((list, fieldValue))
        case Left(invalidState) => Left(invalidState)
      }
    }

    val toSectionFormField: BaseSection => Opt[SectionFormField] = section =>
      SectionHelper
        .atomicFields(section, data)
        .traverse(formFieldByFieldValue)
        .map(ff => SectionFormField(section.shortName.getOrElse(section.title), ff))

    val allSections = RepeatingComponentService.getAllSections(form, formTemplate)

    val visibility = Visibility(allSections, data.mapValues(_.value :: Nil), affinityGroup)
    val filteredSection = allSections.filter(visibility.isVisible)

    val sectionsToSubmit = filteredSection :+ formTemplate.declarationSection
    sectionsToSubmit.traverse(toSectionFormField)
  }

}
