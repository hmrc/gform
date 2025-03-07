/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.retrieval

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import uk.gov.hmrc.auth.core.CredentialRole
import uk.gov.hmrc.crypto.{ Crypted, Decrypter, Encrypter, PlainText }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.retrieval.AuthRetrievals

object EncryptedRetrievalFormat {
  def formatEncrypted(jsonCrypto: Encrypter with Decrypter): Format[AuthRetrievals] = new Format[AuthRetrievals] {
    private def readEncryptedOpt(path: JsPath): Reads[Option[String]] =
      path.readNullable[String].map {
        case Some(s) => Some(Json.parse(jsonCrypto.decrypt(Crypted(s)).value).as[String])
        case _       => None
      }

    private val reads: Reads[AuthRetrievals] = (
      (FormId.format: Reads[FormId]) and
        readEncryptedOpt(__ \ "email") and
        readEncryptedOpt(__ \ "emailLogin") and
        readEncryptedOpt(__ \ "ggLogin") and
        readEncryptedOpt(__ \ "payeNino") and
        readEncryptedOpt(__ \ "ctUtr") and
        readEncryptedOpt(__ \ "saUtr") and
        readEncryptedOpt(__ \ "payeRef") and
        readEncryptedOpt(__ \ "vrn") and
        (__ \ "affinityGroup").read[AffinityGroup] and
        (__ \ "credentialRole").read[CredentialRole]
    )(AuthRetrievals.apply _)

    override def reads(json: JsValue): JsResult[AuthRetrievals] = {
      val authResult: JsResult[AuthRetrievals] = json.validate[AuthRetrievals](reads)
      authResult match {
        case JsSuccess(a, _) => JsSuccess(a)
        case JsError(e)      => JsError("Failed to parse JSON " + e)
      }
    }

    private def writeOpt(os: Option[String]): JsValue =
      os.fold[JsValue](JsNull)(s => JsString(jsonCrypto.encrypt(PlainText(Json.toJson(s).toString())).value))

    override def writes(retrievals: AuthRetrievals): JsValue =
      FormId.format.writes(retrievals._id) ++
        Json.obj("email" -> writeOpt(retrievals.email)) ++
        Json.obj("emailLogin" -> writeOpt(retrievals.emailLogin)) ++
        Json.obj("ggLogin" -> writeOpt(retrievals.ggLogin)) ++
        Json.obj("payeNino" -> writeOpt(retrievals.payeNino)) ++
        Json.obj("ctUtr" -> writeOpt(retrievals.ctUtr)) ++
        Json.obj("saUtr" -> writeOpt(retrievals.saUtr)) ++
        Json.obj("payeRef" -> writeOpt(retrievals.payeRef)) ++
        Json.obj("vrn" -> writeOpt(retrievals.vrn)) ++
        Json.obj("affinityGroup" -> AffinityGroup.format.writes(retrievals.affinityGroup)) ++
        retrievals.credentialRole.toJson.as[JsObject]
  }
}
