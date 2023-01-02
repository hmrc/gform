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

package uk.gov.hmrc.gform.sharedmodel.form

import java.time.Instant
import play.api.libs.json.{ Format, Json, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

case class SignedFormDetails(
  _id: FormId,
  formTemplateId: FormTemplateId,
  envelopeId: EnvelopeId,
  lastUpdated: Instant
)

object SignedFormDetails {
  implicit val formIdFormat: Format[FormId] = FormId.vformat
  implicit val envelopIdFormat: Format[EnvelopeId] = EnvelopeId.vformat
  implicit val instantFormat: Reads[Instant] = MongoJavatimeFormats.instantFormat
  implicit val signedFormDetailsFormat: OFormat[SignedFormDetails] = Json.format
}
