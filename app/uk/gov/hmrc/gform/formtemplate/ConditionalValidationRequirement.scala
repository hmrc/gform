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

sealed abstract class ConditionalValidationRequirement(requiredProperty: String, requiredValues: List[String]) {
  override def toString: String = s"$requiredProperty: [${requiredValues.mkString(", ")}]"
}

case object TypeInfo extends ConditionalValidationRequirement("type", List("info"))

case object TypeChoiceOrRevealingChoice
    extends ConditionalValidationRequirement("type", List("choice", "revealingChoice"))

case object TypeText extends ConditionalValidationRequirement("type", List("text"))

case object MultiLineTrue extends ConditionalValidationRequirement("multiline", List("true"))
