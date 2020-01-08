/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.show._
import cats.syntax.traverse._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.formtemplate.SectionHelper
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object SubmissionServiceHelper {

  def getSectionFormFields(
    form: Form,
    formTemplate: FormTemplate,
    affinityGroup: Option[AffinityGroup]): Opt[List[SectionFormFieldsByAtomicFormComponents]] = {

    val formFieldByFormComponentId: Map[FormComponentId, FormField] =
      form.formData.fields.map(field => field.id -> field).toMap

    def atomicFormComponentFormFields(formComponent: FormComponent): Opt[AtomicFormComponentFormFields] = {
      val consituentFormComponentIds: Opt[NonEmptyList[FormComponentId]] =
        formComponent.`type` match {
          case d: Date        => Right(d.fields(formComponent.id))
          case sc: UkSortCode => Right(sc.fields(formComponent.id))
          case a: Address     => Right(a.fields(formComponent.id))
          case _: Text | _: TextArea | _: Choice | _: HmrcTaxPeriod | _: FileUpload =>
            Right(NonEmptyList.of(formComponent.id))
          case _: Group | _: RevealingChoice | _: InformationMessage =>
            Left(UnexpectedState(
              show"Got an unexpected non-atomic field (${formComponent.id} of type ${formComponent.`type`.getClass.getName})"))
        }

      val formFields: Opt[NonEmptyList[FormField]] =
        consituentFormComponentIds.flatMap {
          _.map(
            id =>
              formFieldByFormComponentId
                .get(id)
                .toRight(UnexpectedState(show"No formField for field.id: ${formComponent.id} found"))).toList
            .partition(_.isLeft) match {
            case (Nil, list) =>
              Right(NonEmptyList.fromListUnsafe(list.collect { case Right(formField) => formField }))
            case (invalidStates, _) =>
              Left(
                UnexpectedState(invalidStates.collect { case Left(invalidState) => invalidState.error }.mkString(", ")))
          }
        }

      formFields.map(formFieldList => AtomicFormComponentFormFields(formComponent, formFieldList))
    }

    def formFieldsByAtomicFormComponents(
      sectionTitle: SmartString,
      fields: List[FormComponent]): Opt[SectionFormFieldsByAtomicFormComponents] =
      SectionHelper
        .atomicLeafFields(fields, formFieldByFormComponentId)
        .traverse(atomicFormComponentFormFields)
        .map(ff => SectionFormFieldsByAtomicFormComponents(sectionTitle, ff))

    val allVisiblePages: List[Page] = RepeatingComponentService.getAllVisiblePages(form, formTemplate, affinityGroup)

    for {
      formSectionFields <- allVisiblePages.traverse(p => formFieldsByAtomicFormComponents(p.title, p.fields))
      declararationSectionFields <- formFieldsByAtomicFormComponents(
                                     formTemplate.declarationSection.title,
                                     formTemplate.declarationSection.fields)
    } yield (declararationSectionFields :: formSectionFields.reverse).reverse
  }
}
