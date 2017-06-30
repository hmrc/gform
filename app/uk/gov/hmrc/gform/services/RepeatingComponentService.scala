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

import uk.gov.hmrc.gform.models.{ FieldId, FieldValue }

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
}
