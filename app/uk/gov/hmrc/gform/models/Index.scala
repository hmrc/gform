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

import play.api.libs.json.{ Json, OFormat, OWrites, Reads }

case class Index(formId: FormId, envelopeId: EnvelopeId)

object Index {

  implicit def format = {
    val mongoIdReads = FormId.format
    val envelopeIdReads = EnvelopeId.format
    val writes = OWrites[Index](index => mongoIdReads.writes(index.formId) ++ Json.obj("envelopeId" -> envelopeIdReads.writes(index.envelopeId)))
    val reads = Reads[Index](json =>
      for {
        id <- mongoIdReads.reads(json)
        data <- envelopeIdReads.reads(json)
      } yield Index(id, data))
    OFormat[Index](reads, writes)
  }
}
