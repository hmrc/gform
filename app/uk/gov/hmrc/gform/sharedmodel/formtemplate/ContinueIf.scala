/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.booleanParser.booleanExprParser

sealed trait ContinueIf
case object Continue extends ContinueIf
case object Stop extends ContinueIf
case class Conditional(expr: BooleanExpr) extends ContinueIf

object ContinueIf {

  private val templateReads: Reads[ContinueIf] = Reads {
    case JsString("true")  => JsSuccess(Continue)
    case JsString("false") => JsSuccess(Stop)
    case json              => booleanExprParser(json).map(Conditional.apply)
  }

  implicit val format: OFormat[ContinueIf] = OFormatWithTemplateReadFallback(templateReads)

}
