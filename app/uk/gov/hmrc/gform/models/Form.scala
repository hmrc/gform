/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import play.api.libs.json._

case class Form(
  _id: FormId,
  formData: FormData
 //TODO envelopeId must go here
)

object Form {
  implicit def format(implicit formDataReads: OFormat[FormData]) = {
    val mongoIdReads = FormIdAsMongoId.format
    val writes = OWrites[Form](form => mongoIdReads.writes(form._id) ++ formDataReads.writes(form.formData))
    val reads = Reads[Form](json =>
      for {
        id <- mongoIdReads.reads(json)
        data <- formDataReads.reads(json)
      } yield Form(id, data))
    OFormat[Form](reads, writes)
  }
}
