/*
 * Copyright 2018 HM Revenue & Customs
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

trait AuthConfigWithAgentAccess {
  def agentAccess: Option[AgentAccess]
}

trait AuthConfigWithEnrolment {
  def serviceId: ServiceId
  def agentAccess: Option[AgentAccess]
  def enrolmentSection: EnrolmentSection
}

case class EEITTAuthConfig(authModule: AuthConfigModule, regimeId: RegimeId) extends AuthConfig

object EEITTAuthConfig {

  val nonAgentIdName = "registrationNumber"
  val agentIdName = "arn"

  implicit val format = Json.format[EEITTAuthConfig]
}

case class HMRCAuthConfigWithAuthModule(authModule: AuthConfigModule, agentAccess: Option[AgentAccess])
    extends AuthConfig with AuthConfigWithAgentAccess

object HMRCAuthConfigWithAuthModule {
  implicit val format = Json.format[HMRCAuthConfigWithAuthModule]
}

case class HMRCAuthConfigWithServiceId(
  authModule: AuthConfigModule,
  agentAccess: Option[AgentAccess],
  serviceId: ServiceId)
    extends AuthConfig with AuthConfigWithAgentAccess

object HMRCAuthConfigWithServiceId {
  implicit val format = Json.format[HMRCAuthConfigWithServiceId]
}

case class HMRCAuthConfigWithRegimeId(
  authModule: AuthConfigModule,
  agentAccess: Option[AgentAccess],
  serviceId: ServiceId,
  regimeId: RegimeId)
    extends AuthConfig with AuthConfigWithAgentAccess
object HMRCAuthConfigWithRegimeId {
  implicit val format = Json.format[HMRCAuthConfigWithRegimeId]
}

case class HMRCAuthConfigWithEnrolment(
  authModule: AuthConfigModule,
  agentAccess: Option[AgentAccess],
  serviceId: ServiceId,
  enrolmentSection: EnrolmentSection)
    extends AuthConfig with AuthConfigWithAgentAccess with AuthConfigWithEnrolment
object HMRCAuthConfigWithEnrolment {
  implicit val format = Json.format[HMRCAuthConfigWithEnrolment]

}

case class HMRCAuthConfig(
  authModule: AuthConfigModule,
  agentAccess: Option[AgentAccess],
  serviceId: ServiceId,
  regimeId: RegimeId,
  enrolmentSection: EnrolmentSection)
    extends AuthConfig with AuthConfigWithAgentAccess with AuthConfigWithEnrolment
object HMRCAuthConfig {
  implicit val format = Json.format[HMRCAuthConfig]

}

object AuthConfig {

  lazy val eeittAuth = "legacyEEITTAuth"
  lazy val hmrcAuth = "hmrc"

  implicit val format: OFormat[AuthConfig] = {
    // format: OFF
    val reads = Reads[AuthConfig] { json =>
      for {
        authModule <- (json \ "authModule").validate[AuthConfigModule]
        regimeId <- (json \ "regimeId").validateOpt[RegimeId]
        serviceId <- (json \ "serviceId").validateOpt[ServiceId]
        agentAccess <- (json \ "agentAccess").validateOpt[AgentAccess]
        enrolmentSection <- (json \ "enrolmentSection").validateOpt[EnrolmentSection]
        result <- (authModule, agentAccess, regimeId, serviceId, enrolmentSection) match {
          case (AuthConfigModule(eeittAuth), None, Some(_), None, None) => EEITTAuthConfig.format.reads(json)
          case (AuthConfigModule(hmrcAuth), _, None, None, None) => HMRCAuthConfigWithAuthModule.format.reads(json)
          case (AuthConfigModule(hmrcAuth), _, None, Some(_), None) => HMRCAuthConfigWithServiceId.format.reads(json)
          case (AuthConfigModule(hmrcAuth), _, Some(_), Some(_), None) => HMRCAuthConfigWithRegimeId.format.reads(json)
          case (AuthConfigModule(hmrcAuth), _, None, Some(_), Some(_)) => HMRCAuthConfigWithEnrolment.format.reads(json)
          case (AuthConfigModule(hmrcAuth), _, Some(_), Some(_), Some(_)) => HMRCAuthConfig.format.reads(json)
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

sealed trait AgentAccess
case object RequireMTDAgentEnrolment extends AgentAccess
case object DenyAnyAgentAffinityUser extends AgentAccess
case object AllowAnyAgentAffinityUser extends AgentAccess
object AgentAccess {
  //  implicit val format: Format[AgentAccess] = derived.oformat[AgentAccess]
  implicit val format: Format[AgentAccess] = new Format[AgentAccess] {
    override def writes(o: AgentAccess): JsValue = o match {
      case RequireMTDAgentEnrolment  => JsString("requireMTDAgentEnrolment")
      case DenyAnyAgentAffinityUser  => JsString("denyAnyAgentAffinityUser")
      case AllowAnyAgentAffinityUser => JsString("allowAnyAgentAffinityUser")
    }

    override def reads(json: JsValue): JsResult[AgentAccess] =
      json match {
        case JsString("")                          => JsSuccess(RequireMTDAgentEnrolment)
        case JsString("requireMTDAgentEnrolment")  => JsSuccess(RequireMTDAgentEnrolment)
        case JsString("denyAnyAgentAffinityUser")  => JsSuccess(DenyAnyAgentAffinityUser)
        case JsString("allowAnyAgentAffinityUser") => JsSuccess(AllowAnyAgentAffinityUser)
        case JsString(err) =>
          JsError(
            s"only three valid agentAccess', requireMTDAgentEnrolment, denyAnyAgentAffinityUser or allowAnyAgentAffinityUser. $err is not valid")
        case _ => JsError("Failure")
      }
  }
}

case class AuthConfigModule(value: String) {
  override def toString: String = value
}

object AuthConfigModule {
  implicit val format: Format[AuthConfigModule] =
    ValueClassFormat.oformat("authModule", AuthConfigModule.apply, _.value)
}
