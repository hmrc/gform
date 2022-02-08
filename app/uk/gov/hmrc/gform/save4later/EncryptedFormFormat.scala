/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.save4later

import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig, PlainText }
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeExpiryDate, EnvelopeId, Form, FormComponentIdToFileIdMapping, FormData, FormId, FormStatus, ThirdPartyData, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateId, FormTemplateVersion }

object EncryptedFormFormat {
  def formatEncrypted(jsonCrypto: CryptoWithKeysFromConfig): Format[Form] = new Format[Form] {
    private val componentIdToFileId = "componentIdToFileId"
    private val formData = "formData"
    private val thirdPartyData = "thirdPartyData"

    override def writes(form: Form): JsValue =
      FormId.format.writes(form._id) ++
        EnvelopeId.format.writes(form.envelopeId) ++
        UserId.oformat.writes(form.userId) ++
        FormTemplateId.oformat.writes(form.formTemplateId) ++
        form.formTemplateVersion.map(FormTemplateVersion.oformat.writes).getOrElse(Json.obj()) ++
        Json.obj(formData -> jsonCrypto.encrypt(PlainText(Json.toJson(form.formData).toString())).value) ++
        FormStatus.oformat.writes(form.status) ++
        VisitIndex.format.writes(form.visitsIndex) ++
        Json.obj(thirdPartyData -> jsonCrypto.encrypt(PlainText(Json.toJson(form.thirdPartyData).toString())).value) ++
        EnvelopeExpiryDate.optionFormat.writes(form.envelopeExpiryDate) ++
        Json.obj(componentIdToFileId -> FormComponentIdToFileIdMapping.format.writes(form.componentIdToFileId))

    private val readFormData: Reads[FormData] =
      (__ \ formData)
        .read[String]
        .map(s => Json.parse(jsonCrypto.decrypt(Crypted(s)).value).as[FormData])

    private val readThirdPartyData: Reads[ThirdPartyData] =
      (__ \ thirdPartyData)
        .read[String]
        .map(s => Json.parse(jsonCrypto.decrypt(Crypted(s)).value).as[ThirdPartyData])

    private val formReads: Reads[Form] = (
      (FormId.format: Reads[FormId]) and
        EnvelopeId.format and
        UserId.oformat and
        FormTemplateId.vformat and
        Form.formTemplateVersionWithFallback and
        readFormData and
        FormStatus.oformat and
        Form.readVisitIndex and
        readThirdPartyData and
        EnvelopeExpiryDate.optionFormat and
        Form.componentIdToFileIdWithFallback
    )(Form.apply _)

    override def reads(json: JsValue): JsResult[Form] = {
      val formResult: JsResult[Form] = json.validate[Form](formReads)
      formResult match {
        case JsSuccess(f, _) => JsSuccess(f)
        case JsError(e)      => JsError("Failed to parse JSON " + e)
      }
    }
  }
}
