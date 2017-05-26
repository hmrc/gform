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

import uk.gov.hmrc.gform.exceptions.InvalidState
import uk.gov.hmrc.gform.models._

object TemplateValidator {

  private def getMandatoryAndOptionalFields(section: Section): (Set[FieldId], Set[FieldId]) = {
    section.atomicFields.foldLeft((Set.empty[FieldId], Set.empty[FieldId])) {
      case ((mandatoryAcc, optionalAcc), field) =>
        (field.`type`, field.mandatory) match {
          case (Address(_), _) => (mandatoryAcc ++ Address.mandatoryFields(field.id), optionalAcc ++ Address.optionalFields(field.id))
          case (Date(_, _, _), _) => (mandatoryAcc ++ Date.fields(field.id), optionalAcc)
          case (Text(_, _), true) => (mandatoryAcc + field.id, optionalAcc)
          case (Text(_, _), false) => (mandatoryAcc, optionalAcc + field.id)
          case (_, true) => (mandatoryAcc + field.id, optionalAcc)
          case (_, false) => (mandatoryAcc, optionalAcc + field.id)
        }
    }
  }

  /**
   * Tries to find section in form template corresponding to submitted data.
   *
   * Section is determined by these rules:
   * - all mandatory fields from the section must be present in submission
   * - optional fields from section don't need to be present in submission
   * - presence of any other field than those from form template is resulting in failed location of a section
   */
  def getMatchingSection(formFields: Seq[FormField], sections: Seq[Section]): Opt[Section] = {
    val formFieldIds: Set[FieldId] = formFields.map(_.id).toSet
    val sectionOpt: Option[Section] = sections.find { section =>

      val (mandatorySectionIds, optionalSectionIds) = getMandatoryAndOptionalFields(section)

      val missingMandatoryFields = mandatorySectionIds diff formFieldIds

      val optionalFieldsFromSubmission = formFieldIds diff mandatorySectionIds

      val fieldWhichAreNotFromFormTemplate = optionalFieldsFromSubmission diff optionalSectionIds

      missingMandatoryFields.isEmpty && fieldWhichAreNotFromFormTemplate.isEmpty
    }

    sectionOpt match {
      case Some(section) => Right(section)
      case None =>
        val sectionsForPrint = sections.map(_.fields.map(_.id))

        Left(InvalidState(s"""|Cannot find a section corresponding to the formFields
                              |FormFields: $formFieldIds
                              |Sections: $sectionsForPrint""".stripMargin))
    }
  }
}
