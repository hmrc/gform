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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.booleanParser.booleanExprParser

case class ValidIf(expr: BooleanExpr)
object ValidIf {
  implicit val writes = Json.writes[ValidIf]

  implicit val reads: Reads[ValidIf] = readsForTemplateJson | readsForMongoJson

  //TODO: move that logic out ot the data

  private lazy val readsForMongoJson = Json.reads[ValidIf]

  private lazy val readsForTemplateJson: Reads[ValidIf] = Reads { json =>
    val includeIfJsR: JsResult[ValidIf] = booleanExprParser(json).map(be => ValidIf(be))
    includeIfJsR
  }
}
