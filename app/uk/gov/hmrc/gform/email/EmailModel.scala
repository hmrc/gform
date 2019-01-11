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

package uk.gov.hmrc.gform.email

import java.util.Date

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }

case class EmailTemplate(
  to: Seq[String], //content that goes in the email to be put into template
  templateId: String, //the template ID that the content will be put into
  parameters: Map[String, String], //the field values that will be passed into the email
  private val force: Boolean = false)

object EmailTemplate {
  implicit val format: OFormat[EmailTemplate] = Json.format[EmailTemplate]
}
