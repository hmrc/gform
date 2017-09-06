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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

sealed trait AuthConfig {
  def authModule: AuthConfigModule
}

case class EEITTAuthConfig(
  authModule: AuthConfigModule,
  regimeId: RegimeId
) extends AuthConfig

object EEITTAuthConfig {
  implicit val format = Json.format[EEITTAuthConfig]
}

case class HMRCAuthConfig(
  authModule: AuthConfigModule,
  regimeId: Option[RegimeId],
  serviceId: Option[ServiceId],
  enrolmentSection: Option[EnrolmentSection]
) extends AuthConfig

object HMRCAuthConfig {

  // format: OFF
  implicit val format = {
    val reads = Reads[HMRCAuthConfig] { json =>
      for {
        authModule       <- (json \ "authModule").validate[AuthConfigModule]
        regimeId         <- (json \ "regimeId").validateOpt[RegimeId]
        serviceId        <- (json \ "serviceId").validateOpt[ServiceId]
        enrolmentSection <- (json \ "enrolmentSection").validateOpt[EnrolmentSection]
        _                <- validateFields(regimeId, serviceId, enrolmentSection)
      } yield HMRCAuthConfig(authModule, regimeId, serviceId, enrolmentSection)
    }

    val writes = Json.writes[HMRCAuthConfig]

    OFormat(reads, writes)
  }
  // format: ON

  private def validateFields(regimeId: Option[RegimeId], serviceId: Option[ServiceId], enrolmentSection: Option[EnrolmentSection]) = {
    (serviceId, regimeId, enrolmentSection) match {
      case (None, Some(_), Some(_)) | (None, None, Some(_)) | (None, Some(_), None) =>
        JsError("serviceId is required when regimeId and/or enrolmentSection are provided")
      case _ => JsSuccess("Success")
    }
  }
}

object AuthConfig {
  implicit val format: OFormat[AuthConfig] = {
    val reads = Reads[AuthConfig] { json =>
      (json \ "authModule").as[AuthConfigModule] match {
        case AuthConfigModule("hmrc") => HMRCAuthConfig.format.reads(json)
        case AuthConfigModule("legacyEEITTAuth") => EEITTAuthConfig.format.reads(json)
        case other => JsError("Unsupported authModule: " + other.value)
      }
    }

    val writes = OWrites[AuthConfig] {
      case conf: HMRCAuthConfig => HMRCAuthConfig.format.writes(conf)
      case conf: EEITTAuthConfig => EEITTAuthConfig.format.writes(conf)
    }

    OFormat(reads, writes)
  }
}

case class ServiceId(value: String)

object ServiceId {
  implicit val format: OFormat[ServiceId] = ValueClassFormat.oformat("value", ServiceId.apply, _.value)
}

case class RegimeId(value: String) {
  override def toString: String = value
}

object RegimeId {

  implicit val format: Format[RegimeId] = ValueClassFormat.oformat("regimeId", RegimeId.apply, _.value)
}

case class AuthConfigModule(value: String) {
  override def toString: String = value
}

object AuthConfigModule {
  implicit val format: Format[AuthConfigModule] = ValueClassFormat.oformat("authModule", AuthConfigModule.apply, _.value)
}
