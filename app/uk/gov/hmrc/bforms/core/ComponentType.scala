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

/**
 * Created by dimitra on 20/03/17.
 */

object ComponentType {

  sealed abstract class EnumTypeId(val name: String) {

    def equalsToEnum(that: String) = this.name.equals(that)

    override def toString = name
  }

  case object Text extends EnumTypeId("text")

  case object Date extends EnumTypeId("date")

  case object Address extends EnumTypeId("address")

  val componentTypes = List(Text, Date, Address)

  implicit val format: OFormat[ComponentType.EnumTypeId] = {
    val formatExpr: OFormat[ComponentType.EnumTypeId] = derived.oformat

    val reads: Reads[ComponentType.EnumTypeId] = (formatExpr: Reads[ComponentType.EnumTypeId]) | Reads {

      case JsString(str: String) =>

        str match {
          case Text.name => JsSuccess(Text)
          case Date.name => JsSuccess(Date)
          case Address.name => JsSuccess(Address)
          case other =>

            JsError(s"Expected one of the following types: $componentTypes, you entered: $other")
        }

      case _ => JsError(s"Expected String as JsValue")
    }

    OFormat[ComponentType.EnumTypeId](reads, formatExpr)
  }

}