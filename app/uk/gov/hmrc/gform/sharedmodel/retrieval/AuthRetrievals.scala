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

package uk.gov.hmrc.gform.sharedmodel.retrieval

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import uk.gov.hmrc.auth.core.CredentialRole
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.form.FormId

case class AuthRetrievals(
  _id: FormId,
  email: Option[String],
  emailLogin: Option[String],
  ggLogin: Option[String],
  payeNino: Option[String],
  ctUtr: Option[String],
  saUtr: Option[String],
  payeRef: Option[String],
  vrn: Option[String],
  affinityGroup: AffinityGroup,
  credentialRole: CredentialRole
)

object AuthRetrievals {
  private def optionFormat[T: Format]: Format[Option[T]] = new Format[Option[T]] {
    override def reads(json: JsValue): JsResult[Option[T]] = json.validateOpt[T]

    override def writes(o: Option[T]): JsValue = o match {
      case Some(t) => implicitly[Writes[T]].writes(t)
      case None    => JsNull
    }
  }

  private val reads: Reads[AuthRetrievals] = (
    (FormId.format: Reads[FormId]) and
      (__ \ "email").readNullable[String] and
      (__ \ "emailLogin").readNullable[String] and
      (__ \ "ggLogin").readNullable[String] and
      (__ \ "payeNino").readNullable[String] and
      (__ \ "ctUtr").readNullable[String] and
      (__ \ "saUtr").readNullable[String] and
      (__ \ "payeRef").readNullable[String] and
      (__ \ "vrn").readNullable[String] and
      (__ \ "affinityGroup").read[AffinityGroup] and
      (__ \ "credentialRole").read[CredentialRole]
  )(AuthRetrievals.apply _)

  private val writes: Writes[AuthRetrievals] = Writes[AuthRetrievals](retrievals =>
    FormId.format.writes(retrievals._id) ++
      Json.obj("email" -> optionFormat[String].writes(retrievals.email)) ++
      Json.obj("emailLogin" -> optionFormat[String].writes(retrievals.emailLogin)) ++
      Json.obj("ggLogin" -> optionFormat[String].writes(retrievals.ggLogin)) ++
      Json.obj("payeNino" -> optionFormat[String].writes(retrievals.payeNino)) ++
      Json.obj("ctUtr" -> optionFormat[String].writes(retrievals.ctUtr)) ++
      Json.obj("saUtr" -> optionFormat[String].writes(retrievals.saUtr)) ++
      Json.obj("payeRef" -> optionFormat[String].writes(retrievals.payeRef)) ++
      Json.obj("vrn" -> optionFormat[String].writes(retrievals.vrn)) ++
      Json.obj("affinityGroup" -> AffinityGroup.format.writes(retrievals.affinityGroup)) ++
      retrievals.credentialRole.toJson.as[JsObject]
  )

  implicit val format: Format[AuthRetrievals] = Format[AuthRetrievals](reads, writes)
}
