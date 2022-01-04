/*
 * Copyright 2022 HM Revenue & Customs
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
import java.time.LocalDate

import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

import scala.collection.immutable.List

object FormTemplateValidatorHelper {

  private def extractDatesFromField(field: FormComponent): List[ConcreteDate] = field.`type` match {
    case group: Group => getAllDatesFromListOfFields(group.fields)
    case _            => getAllDatesFromListOfFields(List(field))
  }

  private def getAllDatesFromFormat(fields: List[FormComponent]): List[ConcreteDate] =
    fields
      .map(_.`type`)
      .collect { case date: Date => date.constraintType }
      .collect { case dateConstraints: DateConstraints => dateConstraints }
      .flatMap(_.constraints)
      .map(_.dateFormat)
      .collect { case concreteDate: ConcreteDate => concreteDate }

  private def getAllDatesFromValue(fields: List[FormComponent]): List[ConcreteDate] =
    fields
      .map(_.`type`)
      .collect { case date: Date => date.value }
      .flatMap(_.collect { case dateValue: DateValue => dateValue }
        .collect {
          case exactDate: ExactDateValue =>
            ConcreteDate(Year.Exact(exactDate.year), Month.Exact(exactDate.month), Day.Exact(exactDate.day))
          case nextDate: NextDateValue =>
            ConcreteDate(Year.Exact(LocalDate.now().getYear + 1), Month.Exact(nextDate.month), Day.Exact(nextDate.day))
          case previousDate: PreviousDateValue =>
            ConcreteDate(
              Year.Exact(LocalDate.now().getYear - 1),
              Month.Exact(previousDate.month),
              Day.Exact(previousDate.day)
            )
        })

  private def getAllDatesFromListOfFields(fields: List[FormComponent]): List[ConcreteDate] =
    getAllDatesFromFormat(fields) ::: getAllDatesFromValue(fields)

  def getAllDates(template: FormTemplate) = {
    val sectionsDates = for {
      formComponent <- template.expandedFormComponentsInMainSections
      date          <- extractDatesFromField(formComponent)
    } yield date

    val acknowledgementAndDeclarationSectionDates = template.destinations match {
      case destinationList: DestinationList =>
        destinationList.acknowledgementSection.fields.flatMap(field => extractDatesFromField(field)) :::
          destinationList.declarationSection.toList.flatMap(_.fields.flatMap(field => extractDatesFromField(field)))

      case _ => Nil
    }

    sectionsDates ::: acknowledgementAndDeclarationSectionDates
  }

}
