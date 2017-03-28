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

package uk.gov.hmrc.bforms.core

import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.collection.immutable._
import uk.gov.hmrc.bforms.models.FieldId

/**
 * Created by dimitra on 20/03/17.
 */

sealed trait ComponentType

case object Text extends ComponentType

case object Date extends ComponentType {
  val fields = (id: FieldId) => List("day", "month", "year").map(id.withSuffix)
}

case object Address extends ComponentType {
  val mandatoryFields = (id: FieldId) => List("street1", "town", "county", "postcode").map(id.withSuffix)
  val optionalFields = (id: FieldId) => List("street2", "street3").map(id.withSuffix)
  val fields = (id: FieldId) => mandatoryFields(id) ++ optionalFields(id)
}

object ComponentType {

  val componentMap: Map[String, ComponentType] =
    Map(
      "text" -> Text,
      "date" -> Date,
      "address" -> Address
    )

  implicit val format: OFormat[ComponentType] = {
    val formatExpr: OFormat[ComponentType] = derived.oformat

    val reads: Reads[ComponentType] = (formatExpr: Reads[ComponentType]) | Reads {

      case JsString(compTypeAsString) =>

        componentMap.get(compTypeAsString) match {
          case Some(componentType) => JsSuccess(componentType)
          case None => JsError(s"Expected one of the following types: ${componentMap.values}, you entered: $compTypeAsString")
        }

      case _ => JsError(s"Expected String as JsValue")
    }

    OFormat[ComponentType](reads, formatExpr)
  }

}
