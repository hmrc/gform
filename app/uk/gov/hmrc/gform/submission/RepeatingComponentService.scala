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

import cats.syntax.eq._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object RepeatingComponentService {

  def getAllVisiblePages(form: Form, formTemplate: FormTemplate, affinityGroup: Option[AffinityGroup]): List[Page] = {
    val formComponentIdsInData = form.formData.fields.map(_.id).toSet
    val variadicData = VariadicFormData.buildFromMongoData(formTemplate, form.formData.toData)

    formTemplate.sections.foldLeft(List.empty[Page]) {
      case (visiblePages, section) =>
        visiblePages ::: getAllVisiblePagesInSection(
          section,
          visiblePages,
          formComponentIdsInData,
          variadicData,
          affinityGroup)
    }
  }

  private def getAllVisiblePagesInSection(
    section: Section,
    previousVisiblePages: List[Page],
    formComponentIdsInData: Set[FormComponentId],
    variadicData: VariadicFormData,
    affinityGroup: Option[AffinityGroup]): List[Page] =
    section match {
      case p: Section.NonRepeatingPage =>
        if (isIncluded(p.page.includeIf, previousVisiblePages, variadicData, affinityGroup)) List(p.page)
        else Nil

      case rp: Section.RepeatingPage =>
        if (isIncluded(rp.page.includeIf, previousVisiblePages, variadicData, affinityGroup))
          reconstructRepeatingPage(rp.page, formComponentIdsInData)
        else Nil

      case lf: Section.AddToList => Nil // ToDo: Lance - implement this
    }

  private def isIncluded(
    includeIf: Option[IncludeIf],
    previousVisiblePages: List[Page],
    variadicData: VariadicFormData,
    affinityGroup: Option[AffinityGroup]): Boolean =
    includeIf
      .map(incIf => BooleanExprEval.isTrue(incIf.expr, variadicData, affinityGroup))
      .map { res =>
        res.beResult &&
        res.dependingOn.forall(formComponentId =>
          previousVisiblePages.exists(_.fields.exists(_.id === formComponentId)))
      }
      .getOrElse(true)

  private def reconstructRepeatingPage(page: Page, formComponentIdsInData: Set[FormComponentId]): List[Page] = {
    def getIdsOfLeafFormComponents(field: FormComponent): List[FormComponentId] = field.`type` match {
      case g: Group => g.fields.flatMap(getIdsOfLeafFormComponents)
      case rc: RevealingChoice =>
        field.id :: rc.options.toList.flatMap(_.revealingFields.flatMap(getIdsOfLeafFormComponents))
      case _: Text | _: TextArea | _: UkSortCode | _: Date | _: Address | _: Address | _: Choice | _: HmrcTaxPeriod |
          _: InformationMessage | _: FileUpload =>
        List(field.id)
    }

    // ToDo: Lance - this won't work if the first field in the section is an Information field
    val selector = page.fields.flatMap(getIdsOfLeafFormComponents).head.value
    val count = formComponentIdsInData.count(formComponentId => formComponentId.value.endsWith(selector))
    (1 to count).map { i =>
      copyPage(page, i)
    }.toList
  }

  private def copyPage(section: Page, index: Int) = {
    def copyField(field: FormComponent): FormComponent =
      field.`type` match {
        case grp: Group =>
          field
            .copy(id = field.id.extendForRepeatingSection(index), `type` = grp.copy(fields = grp.fields.map(copyField)))
        case rc: RevealingChoice =>
          field.copy(
            id = field.id.extendForRepeatingSection(index),
            `type` = rc.copy(
              options = rc.options.map(option => option.copy(revealingFields = option.revealingFields.map(copyField))))
          )
        case _: Text | _: TextArea | _: UkSortCode | _: Date | _: Address | _: Address | _: Choice | _: HmrcTaxPeriod |
            _: InformationMessage | _: FileUpload =>
          field.copy(id = field.id.extendForRepeatingSection(index))
      }

    section.copy(
      title = buildText(section.title, index),
      shortName = optBuildText(section.shortName, index),
      fields = section.fields.map(copyField))
  }

  private def optBuildText(maybeLs: Option[SmartString], index: Int): Option[SmartString] =
    maybeLs.map(ls => buildText(ls, index))

  private def buildText(ls: SmartString, index: Int): SmartString =
    ls.replace("$n", index.toString)
}
