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

package uk.gov.hmrc.gform.submissionconsolidator

import play.api.libs.json.{ Format, Json }

case class APIFormField(id: String, value: String)

object APIFormField {
  implicit val formats = Json.format[APIFormField]
}

case class APIForm(
  submissionRef: String,
  projectId: String,
  templateId: String,
  customerId: String,
  submissionTimestamp: String,
  formData: List[APIFormField]
)

object APIForm {
  implicit val formats: Format[APIForm] = Json.format[APIForm]
}

case class APIFieldError(path: String, message: String) {
  def formatted = s"$path=$message"
}
object APIFieldError {
  implicit val formats: Format[APIFieldError] = Json.format[APIFieldError]
}

case class APIError(code: String, message: String, fieldErrors: List[APIFieldError] = List.empty) {
  def formatted = s"code=$code, message=$message, fieldErrors=[${fieldErrors.map(_.formatted).mkString(",")}]"
}
object APIError {
  implicit val formats: Format[APIError] = Json.format[APIError]
}
