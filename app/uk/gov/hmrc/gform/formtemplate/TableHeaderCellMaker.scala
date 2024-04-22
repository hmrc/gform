/*
 * Copyright 2024 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TableHeaderCell

class TableHeaderCellMaker(json: JsValue) {
  def optTableHeaderCell(): Opt[TableHeaderCell] =
    json match {
      case JsObject(_) =>
        val classes: Option[String] = (json \ "classes").asOpt[String]
        val enField = getDownField("en", json)
        val cyField = getDownField("cy", json)

        if (enField.fields.isEmpty) {
          Left(UnexpectedState("'en' is missing in table header"))
        } else {
          for {
            label <- (enField ++ cyField).asOpt[SmartString].toRight(UnexpectedState("Table header is not correct"))
          } yield TableHeaderCell(label, classes)

        }
      case _ =>
        for {
          label <- json.asOpt[SmartString].toRight(UnexpectedState("Table header is not correct"))
        } yield TableHeaderCell(label, None)
    }

  private def getDownField(fieldName: String, json: JsValue): JsObject =
    (json \ fieldName) match {
      case JsDefined(field) => Json.obj(fieldName -> field)
      case _: JsUndefined   => Json.obj()
    }
}
