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

sealed abstract class ConditionalValidationRequirement(requiredProperty: String, requiredValue: String) {

  def getRequiredProperty: String = requiredProperty

  def getRequiredValue: String = requiredValue
}

object ConditionalValidationRequirement {

  case object TypeInfo extends ConditionalValidationRequirement("type", "info")

  case object TypeChoice extends ConditionalValidationRequirement("type", "choice")

  case object TypeRevealingChoice extends ConditionalValidationRequirement("type", "revealingChoice")

  case object TypeText extends ConditionalValidationRequirement("type", "text")

  case object TypeDate extends ConditionalValidationRequirement("type", "date")

  case object TypeGroup extends ConditionalValidationRequirement("type", "group")

  case object TypeAddress extends ConditionalValidationRequirement("type", "address")

  case object TypeOverseasAddress extends ConditionalValidationRequirement("type", "overseasAddress")

  case object TypePostcodeLookup extends ConditionalValidationRequirement("type", "postcodeLookup")

  case object MultilineTrue extends ConditionalValidationRequirement("multiline", "true")

}
