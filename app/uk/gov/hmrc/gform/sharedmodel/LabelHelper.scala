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

package uk.gov.hmrc.gform.sharedmodel

import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

object LabelHelper {
  def buildRepeatingLabel(field: FormComponent, index: Int) =
    field.label.copy(m = field.label.m.map { case (lang, message) => (lang, message.replace("$n", index.toString)) })

  def buildRepeatingLabel(shortName: Option[LocalisedString], index: Int): Option[LocalisedString] =
    shortName.map(ls => ls.copy(m = ls.m.map { case (lang, message) => (lang, message.replace("$n", index.toString)) }))
}
