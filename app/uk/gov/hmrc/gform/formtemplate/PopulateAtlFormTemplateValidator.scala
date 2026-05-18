/*
 * Copyright 2026 HM Revenue & Customs
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

import uk.gov.hmrc.gform.core.ValidationResult
import uk.gov.hmrc.gform.sharedmodel.{ Attr, DataRetrieve, DataRetrieveDefinitions }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataRetrieveCtx, FormTemplate, Page, Section }

import scala.collection.mutable.ArrayBuffer

class PopulateAtlFormTemplateValidator(private val formTemplate: FormTemplate, pages: List[Page]) {

  private lazy val atlMap = formTemplate.formKind.allSections.collect { case atl: Section.AddToList =>
    atl.pageId.id -> atl
  }.toMap

  def validate(): ValidationResult = {
    val dataRetrieves = pages.flatMap(_.dataRetrieves())
    val errors = dataRetrieves
      .map(validateDataRetrieve)
      .foldLeft(new StringBuilder()) {
        case (acc, List()) => acc
        case (acc, errors) => errors.foldLeft(acc) { case (acc, error) => acc.addAll(error + "; ") }
      }

    errors.toString() match {
      case ""    => uk.gov.hmrc.gform.core.Valid
      case error => uk.gov.hmrc.gform.core.Invalid(error)
    }
  }

  private def validateDataRetrieve(dataRetrieve: DataRetrieve): List[String] = {
    val errors = ArrayBuffer[String]()
    dataRetrieve.populateATL.foreach { populateAtl =>
      val atlId = populateAtl.id
      val mappingFields = populateAtl.mapping.keys

      val atl = atlMap.get(atlId)

      if (atl.isEmpty)
        errors.addOne(s"dataRetrieve ${dataRetrieve.id.value}: is referencing an ATL id ($atlId) that does not exist")

      atl.foreach { atl =>
        def mappingFieldContainedInAtl(fieldId: String): Boolean =
          atl.pages.toList
            .flatMap(_.fields)
            .map(_.id.value)
            .contains(fieldId)

        mappingFields
          .map(mappingFieldContainedInAtl)
          .zip(mappingFields)
          .collect { case (false, fieldId) =>
            errors.addOne(
              s"dataRetrieve ${dataRetrieve.id.value}: mapping field $fieldId is not available inside of ATL $atlId"
            )
          }

        val expressions = populateAtl.mapping.values

        expressions
          .flatMap(expr => expr.leafs())
          .collect {
            case DataRetrieveCtx(id, attribute) if id == dataRetrieve.id =>
              val dataRetrieveDefinition = DataRetrieveDefinitions.staticDefinitions.definitions
                .find(_.tpe == dataRetrieve.tpe)
                .get
              dataRetrieveDefinition.attributes match {
                case Attr.FromObject(inst) =>
                  errors.addOne(
                    s"dataRetrieve ${dataRetrieve.id.value} of type ${dataRetrieve.tpe} is an object data retrieve. Only array dataRetrieves are supported"
                  )
                case Attr.FromArray(inst) =>
                  val containsAttribute = dataRetrieveDefinition.attrTypeMapping.keys.toList.contains(attribute)
                  if (!containsAttribute) {
                    errors.addOne(
                      s"dataRetrieve ${dataRetrieve.id.value}: dataRetrieve.${dataRetrieve.id.value}.${attribute.name} does not exist for dataRetrieve type of ${dataRetrieve.tpe.name}"
                    )
                  }
              }
          }
      }
    }
    errors.toList
  }
}
