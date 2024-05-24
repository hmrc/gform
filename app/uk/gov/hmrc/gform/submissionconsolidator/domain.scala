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

package uk.gov.hmrc.gform.submissionconsolidator

import play.api.libs.json.{ Format, Json }
import play.api.libs.json.OFormat

case class SCFormField(id: String, value: String)

object SCFormField {
  implicit val formats: OFormat[SCFormField] = Json.format[SCFormField]
}

case class SCForm(
  submissionRef: String,
  projectId: String,
  templateId: String,
  customerId: String,
  submissionTimestamp: String,
  formData: Seq[SCFormField]
)

object SCForm {
  implicit val formats: Format[SCForm] = Json.format[SCForm]
}

case class SCFieldError(path: String, message: String) {
  def formatted = s"$path=$message"
}
object SCFieldError {
  implicit val formats: Format[SCFieldError] = Json.format[SCFieldError]
}

case class SCError(code: String, message: String, fieldErrors: List[SCFieldError] = List.empty) {
  def formatted = s"code=$code, message=$message, fieldErrors=[${fieldErrors.map(_.formatted).mkString(",")}]"
}
object SCError {
  implicit val formats: Format[SCError] = Json.format[SCError]
}
