/*
 * Copyright 2023 HM Revenue & Customs
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

case class ValidIf(booleanExpr: BooleanExpr)
object ValidIf {

  private val templateReads: Reads[ValidIf] = Reads(json => booleanExprParser(json).map(ValidIf.apply))

  implicit val format: OFormat[ValidIf] = OFormatWithTemplateReadFallback(templateReads)

  implicit val leafExprs: LeafExpr[ValidIf] = (path: TemplatePath, t: ValidIf) => LeafExpr(path, t.booleanExpr)

}
