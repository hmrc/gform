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

trait AuthConfigWithEnrolment {
  def serviceId: ServiceId
  def enrolmentSection: EnrolmentSection
}

case class EEITTAuthConfig(
  authModule: AuthConfigModule,
  regimeId: RegimeId
) extends AuthConfig

object EEITTAuthConfig {
  implicit val format = Json.format[EEITTAuthConfig]
}

case class HMRCAuthConfigWithAuthModule(
  authModule: AuthConfigModule
) extends AuthConfig

object HMRCAuthConfigWithAuthModule {
  implicit val format = Json.format[HMRCAuthConfigWithAuthModule]
}

case class HMRCAuthConfigWithServiceId(
  authModule: AuthConfigModule,
  serviceId: ServiceId
) extends AuthConfig

object HMRCAuthConfigWithServiceId {
  implicit val format = Json.format[HMRCAuthConfigWithServiceId]
}

case class HMRCAuthConfigWithRegimeId(
  authModule: AuthConfigModule,
  serviceId: ServiceId,
  regimeId: RegimeId
) extends AuthConfig
object HMRCAuthConfigWithRegimeId {
  implicit val format = Json.format[HMRCAuthConfigWithRegimeId]
}

case class HMRCAuthConfigWithEnrolment(
  authModule: AuthConfigModule,
  serviceId: ServiceId,
  enrolmentSection: EnrolmentSection
) extends AuthConfig with AuthConfigWithEnrolment
object HMRCAuthConfigWithEnrolment {
  implicit val format = Json.format[HMRCAuthConfigWithEnrolment]

}

case class HMRCAuthConfig(
  authModule: AuthConfigModule,
  serviceId: ServiceId,
  regimeId: RegimeId,
  enrolmentSection: EnrolmentSection
) extends AuthConfig with AuthConfigWithEnrolment
object HMRCAuthConfig {
  implicit val format = Json.format[HMRCAuthConfig]

}

object AuthConfig {
  implicit val format: OFormat[AuthConfig] = {
    // format: OFF
    val reads = Reads[AuthConfig] { json =>
      for {
        authModule       <- (json \ "authModule").validate[AuthConfigModule]
        regimeId         <- (json \ "regimeId").validateOpt[RegimeId]
        serviceId        <- (json \ "serviceId").validateOpt[ServiceId]
        enrolmentSection <- (json \ "enrolmentSection").validateOpt[EnrolmentSection]
        result           <- (authModule, regimeId, serviceId, enrolmentSection) match {
          case (AuthConfigModule("legacyEEITTAuth"), Some(_), None, None) => EEITTAuthConfig.format.reads(json)
          case (AuthConfigModule("hmrc"), None,    None,    None)    => HMRCAuthConfigWithAuthModule.format.reads(json)
          case (AuthConfigModule("hmrc"), None,    Some(_), None)    => HMRCAuthConfigWithServiceId.format.reads(json)
          case (AuthConfigModule("hmrc"), Some(_), Some(_), None)    => HMRCAuthConfigWithRegimeId.format.reads(json)
          case (AuthConfigModule("hmrc"), None,    Some(_), Some(_)) => HMRCAuthConfigWithEnrolment.format.reads(json)
          case (AuthConfigModule("hmrc"), Some(_), Some(_), Some(_)) => HMRCAuthConfig.format.reads(json)
          case _ => JsError("")
        }
      } yield result
    }

    val writes = OWrites[AuthConfig] {
      case conf: EEITTAuthConfig              => EEITTAuthConfig.format.writes(conf)
      case conf: HMRCAuthConfigWithAuthModule => HMRCAuthConfigWithAuthModule.format.writes(conf)
      case conf: HMRCAuthConfigWithServiceId  => HMRCAuthConfigWithServiceId.format.writes(conf)
      case conf: HMRCAuthConfigWithRegimeId   => HMRCAuthConfigWithRegimeId.format.writes(conf)
      case conf: HMRCAuthConfigWithEnrolment  => HMRCAuthConfigWithEnrolment.format.writes(conf)
      case conf: HMRCAuthConfig               => HMRCAuthConfig.format.writes(conf)
    }
    // format: ON
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
