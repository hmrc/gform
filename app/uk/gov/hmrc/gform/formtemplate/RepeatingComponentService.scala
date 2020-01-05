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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object RepeatingComponentService {

  def discardRepeatingFields(
    extraFieldsReceived: Set[FormComponentId],
    mandatoryFields: Set[FormComponentId],
    optionalFields: Set[FormComponentId]): Set[FormComponentId] =
    extraFieldsReceived.filterNot { extraFieldId =>
      (mandatoryFields ++ optionalFields).exists { fieldId =>
        val pattern = s"""^\\d+_${fieldId.value}""".r
        pattern.findFirstIn(extraFieldId.value).isDefined
      }
    }

  def findTemplateFieldId(
    fieldMap: Map[FormComponentId, FormComponent],
    fieldId: FormComponentId): Option[FormComponent] =
    fieldMap.get(fieldId.reduceToTemplateFieldId)

  def getAllSections(form: Form, formTemplate: FormTemplate): List[Section] =
    formTemplate.sections.flatMap { section =>
      if (section.isRepeating) {
        val data = form.formData.fields.map(field => field.id.value -> field.value).toMap
        reconstructRepeatingSections(section, data)
      } else {
        List(section)
      }
    }

  private def reconstructRepeatingSections(section: Section, data: Map[String, String]): List[Section] = {
    def getFields(field: FormComponent): List[String] = field.`type` match {
      case Group(fields, _, _, _, _, _) => fields.flatMap(getFields)
      case _                            => List(field.id.value)
    }

    val selector = section.fields.flatMap(getFields).head
    val count = data.keys.count(field => field.endsWith(selector))
    (1 to count).map { i =>
      copySection(section, i, data)
    }.toList
  }

  private def copySection(section: Section, index: Int, data: Map[String, String]) = {
    def copyField(field: FormComponent): FormComponent =
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) =>
          field.copy(
            id = FormComponentId(s"${index}_${field.id.value}"),
            `type` = grp.copy(fields = fields.map(copyField)))
        case _ => field.copy(id = FormComponentId(s"${index}_${field.id.value}"))
      }

    section.copy(
      title = buildText(section.title, index, data),
      shortName = optBuildText(section.shortName, index, data),
      fields = section.fields.map(copyField))
  }

  private def optBuildText(maybeLs: Option[SmartString], index: Int, data: Map[String, String]): Option[SmartString] =
    maybeLs.map(ls => buildText(ls, index, data))

  private def buildText(ls: SmartString, index: Int, data: Map[String, String]): SmartString =
    ls.replace("$n", index.toString)
}
