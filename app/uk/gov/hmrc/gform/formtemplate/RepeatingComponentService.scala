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

package uk.gov.hmrc.gform.formtemplate

import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object RepeatingComponentService {

  def discardRepeatingFields(
    extraFieldsReceived: Set[FormComponentId],
    mandatoryFields: Set[FormComponentId],
    optionalFields: Set[FormComponentId]) =
    extraFieldsReceived.filterNot { extraFieldId =>
      (mandatoryFields ++ optionalFields).exists { fieldId =>
        val pattern = s"""^\\d+_${fieldId.value}""".r
        pattern.findFirstIn(extraFieldId.value).isDefined
      }
    }

  def findTemplateFieldId(fieldMap: Map[FormComponentId, FormComponent], fieldId: FormComponentId) = {
    val repeatingGroupFieldId = """^\d+_(.+)""".r

    val templateFieldId = fieldId.value match {
      case repeatingGroupFieldId(extractedFieldId) => FormComponentId(extractedFieldId)
      case _                                       => fieldId
    }

    fieldMap.get(templateFieldId)
  }

  def getAllSections(form: Form, formTemplate: FormTemplate): List[Section] =
    formTemplate.sections.flatMap { section =>
      if (isRepeatingSection(section)) {
        val data = form.formData.fields.map(field => field.id.value -> field.value).toMap
        reconstructRepeatingSections(section, data)
      } else {
        List(section)
      }
    }

  private def isRepeatingSection(section: Section) = section.repeatsMax.isDefined && section.repeatsMin.isDefined

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
      title = buildText(Some(section.title), index, data).getOrElse(""),
      shortName = buildText(section.shortName, index, data),
      fields = section.fields.map(copyField))
  }

  private def buildText(template: Option[String], index: Int, data: Map[String, String]): Option[String] = {

    def evaluateTextExpression(str: String) = {
      val field = str.replaceFirst("""\$\{""", "").replaceFirst("""\}""", "")
      if (field.startsWith("n_")) { // "n_"  in the expression means nth instance a field in a repeating group
        if (index == 1) { // the first element in a repeating group doesn't have an index
          val fieldName = field.replaceFirst("n_", "")
          data.getOrElse(fieldName, "")
        } else {
          val fieldName = field.replaceFirst("n_", s"${index - 1}_")
          data.getOrElse(fieldName, "")
        }
      } else {
        // depending on the specified form validation we might have an empty string
        // in which case it means deleting the expression from the final text
        data.getOrElse(field, "")
      }
    }

    def getEvaluatedText(str: String) = {
      val pattern = """.*(\$\{.*\}).*""".r
      val expression = str match {
        case pattern(txtExpr) => txtExpr
        case _                => ""
      }
      val evaluatedText = evaluateTextExpression(expression)
      str.replace(expression, evaluatedText)
    }

    template match {
      case Some(inputText) => Some(getEvaluatedText(inputText).replace("$n", index.toString))
      case _               => None
    }
  }
}
