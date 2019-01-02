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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.functional._
import play.api.libs.json._
import play.api.libs.json.Writes._
import play.api.libs.json.Reads._

case class RepeatingGroupStructure(structure: Map[String, JsValue])

object RepeatingGroupStructure {

  implicit val format: OFormat[RepeatingGroupStructure] = Json.format[RepeatingGroupStructure]

  implicit val optionFormat: OFormat[Option[RepeatingGroupStructure]] = new OFormat[Option[RepeatingGroupStructure]] {
    override def writes(o: Option[RepeatingGroupStructure]): JsObject =
      o match {
        case Some(x) => Json.obj("structure" -> Json.toJson(x.structure))
        case None    => Json.obj()
      }

    override def reads(json: JsValue) =
      json.\("structure").asOpt[Map[String, JsValue]] match {
        case Some(x) => JsSuccess(Some(RepeatingGroupStructure(x)))
        case None    => JsSuccess(None)
      }
  }

  optionFormat.writes(Some(RepeatingGroupStructure(Map.empty[String, JsValue])))
}
