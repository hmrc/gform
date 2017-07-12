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

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Form(
  _id: FormId,
  formData: FormData,
  envelopeId: EnvelopeId
)

object Form {

  //  implicit def format = {
  //    val mongoIdReads = FormIdAsMongoId.format
  //    val writes = OWrites[Form](form => mongoIdReads.writes(form._id) ++ FormData.format.writes(form.formData) ++ EnvelopeId.format.writes(form.envelopeId))
  //    val reads = Reads[Form](json =>
  //      for {
  //        id <- mongoIdReads.reads(json)
  //        data <- FormData.format.reads(json)
  //        envelopeId <- EnvelopeId.format.reads(json)
  //      } yield Form(id, data, envelopeId))
  //    OFormat[Form](reads, writes)
  //  }
  //
  private val reads: Reads[Form] = (
    (FormId.format: Reads[FormId]) and
    FormData.format and
    EnvelopeId.format
  )(Form.apply _)

  private val writes: OWrites[Form] = OWrites[Form](form =>
    FormId.format.writes(form._id) ++
      FormData.format.writes(form.formData) ++
      Json.obj("envelopeId" -> EnvelopeId.format.writes(form.envelopeId)))

  implicit val format: OFormat[Form] = OFormat[Form](reads, writes)
}
