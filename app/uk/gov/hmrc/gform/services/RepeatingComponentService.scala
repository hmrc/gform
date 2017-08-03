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

import uk.gov.hmrc.gform.models._

object RepeatingComponentService {

  def discardRepeatingFields(extraFieldsReceived: Set[FieldId], mandatoryFields: Set[FieldId], optionalFields: Set[FieldId]) = {
    extraFieldsReceived.filterNot { extraFieldId =>
      (mandatoryFields ++ optionalFields).exists { fieldId =>
        val pattern = s"""^\\d+_${fieldId.value}""".r
        pattern.findFirstIn(extraFieldId.value).isDefined
      }
    }
  }

  def findTemplateFieldId(fieldMap: Map[FieldId, FieldValue], fieldId: FieldId) = {
    val repeatingGroupFieldId = """^\d+_(.+)""".r

    val templateFieldId = fieldId.value match {
      case repeatingGroupFieldId(extractedFieldId) => FieldId(extractedFieldId)
      case _ => fieldId
    }

    fieldMap.get(templateFieldId)
  }

  def getAllSections(form: Form, formTemplate: FormTemplate): List[Section] = {
    formTemplate.sections.flatMap { section =>
      if (isRepeatingSection(section)) {
        val data = form.formData.fields.map(field => field.id.value -> field.value).toMap
        reconstructRepeatingSections(section, data)
      } else {
        List(section)
      }
    }
  }

  private def isRepeatingSection(section: Section) = section.repeatsMax.isDefined && section.fieldToTrack.isDefined

  private def reconstructRepeatingSections(section: Section, data: Map[String, String]): List[Section] = {
    def getFields(field: FieldValue): List[String] = field.`type` match {
      case Group(fields, _, _, _, _, _) => fields.flatMap(getFields)
      case _ => List(field.id.value)
    }

    val selector = section.fields.flatMap(getFields).head
    val count = data.keys.count(field => field.endsWith(selector))
    (1 to count).map { i =>
      copySection(section, i, data)
    }.toList
  }

  private def copySection(section: Section, index: Int, data: Map[String, String]) = {
    def copyField(field: FieldValue): FieldValue = {
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) => field.copy(
          id = FieldId(s"${index}_${field.id.value}"),
          `type` = grp.copy(fields = fields.map(copyField))
        )
        case _ => field.copy(
          id = FieldId(s"${index}_${field.id.value}")
        )
      }
    }

    section.copy(
      title = buildText(Some(section.title), index, section.fieldToTrack.get, data).getOrElse(""),
      shortName = buildText(section.shortName, index, section.fieldToTrack.get, data),
      fields = section.fields.map(copyField)
    )
  }

  private def buildText(template: Option[String], index: Int, fieldToTrack: VariableInContext,
    data: Map[String, String]): Option[String] = {

    val fieldName = if (index == 1) fieldToTrack.field else s"${index - 1}_${fieldToTrack.field}"
    val textToInsert = data.getOrElse(fieldName, "")

    template match {
      case Some(text) => Some(
        text.replace("$t", textToInsert).replace("$n", index.toString)
      )
      case None => None
    }
  }
}
