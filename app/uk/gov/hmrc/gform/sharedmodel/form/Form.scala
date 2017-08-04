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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

case class Form(
  _id: FormId,
  envelopeId: EnvelopeId,
  userId: UserId,
  formTemplateId: FormTemplateId,
  repeatingGroupStructure: Option[RepeatingGroupStructure],
  formData: FormData
)

object Form {

  private val reads: Reads[Form] = (
    (FormId.format: Reads[FormId]) and
    EnvelopeId.format and
    UserId.oformat and
    FormTemplateId.vformat and
    RepeatingGroupStructure.optionFormat and
    FormData.format
  )(Form.apply _)

  private val writes: OWrites[Form] = OWrites[Form](form =>
    FormId.format.writes(form._id) ++
      EnvelopeId.format.writes(form.envelopeId) ++
      UserId.oformat.writes(form.userId) ++
      FormTemplateId.oformat.writes(form.formTemplateId) ++
      RepeatingGroupStructure.optionFormat.writes(form.repeatingGroupStructure) ++
      FormData.format.writes(form.formData))

  implicit val format: OFormat[Form] = OFormat[Form](reads, writes)

}
