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

import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.auth.core.{ AffinityGroup => CoreAffinityGroup }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

object EEITTAuthConfig {
  val nonAgentIdName = "registrationNumber"
  val agentIdName = "arn"
}

case class EnrolmentAuth(
  serviceId: ServiceId,
  enrolmentCheck: EnrolmentCheck
)
object EnrolmentAuth {
  implicit val format: OFormat[EnrolmentAuth] = derived.oformat
}

sealed trait EnrolmentCheck extends Product with Serializable
case class DoCheck(
  enrolmentCheckPredicate: EnrolmentCheckPredicate,
  needEnrolment: NeedEnrolment,
  check: EnrolmentPostCheck
) extends EnrolmentCheck
case object Never extends EnrolmentCheck
object EnrolmentCheck {
  implicit val format: OFormat[EnrolmentCheck] = derived.oformat
}

sealed trait EnrolmentCheckPredicate extends Product with Serializable
case object Always extends EnrolmentCheckPredicate
case object ForNonAgents extends EnrolmentCheckPredicate
object EnrolmentCheckPredicate {
  implicit val format: OFormat[EnrolmentCheckPredicate] = derived.oformat
}

sealed trait NeedEnrolment extends Product with Serializable
case class RequireEnrolment(enrolmentSection: EnrolmentSection) extends NeedEnrolment
case object RejectAccess extends NeedEnrolment
object NeedEnrolment {
  implicit val format: OFormat[NeedEnrolment] = derived.oformat
}

sealed trait EnrolmentPostCheck extends Product with Serializable
case object NoCheck extends EnrolmentPostCheck
case class RegimeIdCheck(regimeId: RegimeId) extends EnrolmentPostCheck
object EnrolmentPostCheck {
  implicit val format: OFormat[EnrolmentPostCheck] = derived.oformat
}

sealed trait EnrolmentCheckVerb extends Product with Serializable
case object NeverVerb extends EnrolmentCheckVerb
case object AlwaysVerb extends EnrolmentCheckVerb
case object ForNonAgentsVerb extends EnrolmentCheckVerb
object EnrolmentCheckVerb {

  private val never = "never"
  private val always = "always"
  private val forNonAgents = "forNonAgents"

  implicit val format: Format[EnrolmentCheckVerb] = new Format[EnrolmentCheckVerb] {
    override def writes(o: EnrolmentCheckVerb): JsValue = o match {
      case NeverVerb        => JsString(never)
      case AlwaysVerb       => JsString(always)
      case ForNonAgentsVerb => JsString(forNonAgents)
    }

    override def reads(json: JsValue): JsResult[EnrolmentCheckVerb] =
      json match {
        case JsString(`never`)        => JsSuccess(NeverVerb)
        case JsString(`always`)       => JsSuccess(AlwaysVerb)
        case JsString(`forNonAgents`) => JsSuccess(ForNonAgentsVerb)
        case invalid =>
          JsError(s"enrolmentCheck is invalid. Expected one of: $always, $never or $forNonAgents, but got $invalid.")
      }
  }
}

sealed trait AuthModule extends Product with Serializable
case object Hmrc extends AuthModule
case object EeittLegacy extends AuthModule

object AuthModule {

  private val hmrc = "hmrc"
  private val legacyEEITTAuth = "legacyEEITTAuth"

  implicit val format: Format[AuthModule] = new Format[AuthModule] {
    override def writes(o: AuthModule): JsValue = o match {
      case Hmrc        => JsString(hmrc)
      case EeittLegacy => JsString(legacyEEITTAuth)
    }

    override def reads(json: JsValue): JsResult[AuthModule] =
      json match {
        case JsString(`hmrc`)            => JsSuccess(Hmrc)
        case JsString(`legacyEEITTAuth`) => JsSuccess(EeittLegacy)
        case invalid =>
          JsError(s"authModule is invalid. Expected one of: $hmrc or $legacyEEITTAuth, but got $invalid.")
      }
  }
}

sealed trait AuthConfig extends Product with Serializable
case class EeittModule(regimeId: RegimeId) extends AuthConfig
case object HmrcSimpleModule extends AuthConfig
case class HmrcEnrolmentModule(enrolmentAuth: EnrolmentAuth) extends AuthConfig
case class HmrcAgentModule(agentAccess: AgentAccess) extends AuthConfig
case class HmrcAgentWithEnrolmentModule(agentAccess: AgentAccess, enrolmentAuth: EnrolmentAuth) extends AuthConfig

object AuthConfig {

  def toEnrolmentPostCheck(maybeRegimeId: Option[RegimeId]): EnrolmentPostCheck =
    maybeRegimeId.fold(NoCheck: EnrolmentPostCheck)(RegimeIdCheck.apply)

  def toEnrolmentAuth(
    serviceId: ServiceId,
    maybeRegimeId: Option[RegimeId],
    maybeEnrolmentCheck: Option[EnrolmentCheckVerb],
    maybeEnrolmentSection: Option[EnrolmentSection]): Either[String, EnrolmentAuth] =
    (maybeEnrolmentCheck, maybeEnrolmentSection) match {
      case (Some(NeverVerb), _) => Right(EnrolmentAuth(serviceId, Never))
      case (Some(AlwaysVerb), Some(enrolmentSection)) =>
        Right(
          EnrolmentAuth(
            serviceId,
            DoCheck(Always, RequireEnrolment(enrolmentSection), toEnrolmentPostCheck(maybeRegimeId))))
      case (Some(ForNonAgentsVerb), Some(enrolmentSection)) =>
        Right(
          EnrolmentAuth(
            serviceId,
            DoCheck(ForNonAgents, RequireEnrolment(enrolmentSection), toEnrolmentPostCheck(maybeRegimeId))))
      case (Some(AlwaysVerb), None) =>
        Right(EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, toEnrolmentPostCheck(maybeRegimeId))))
      case (Some(ForNonAgentsVerb), None) =>
        Right(EnrolmentAuth(serviceId, DoCheck(ForNonAgents, RejectAccess, toEnrolmentPostCheck(maybeRegimeId))))
      case (None, _) => Left("enrolmentCheck not provided. Expected one of 'never', 'always' or 'forNonAgents'")
    }

  implicit val format: OFormat[AuthConfig] = {
    val rawTemplateReads = Reads[AuthConfig] { json =>
      for {
        authModule            <- (json \ "authModule").validate[AuthModule]
        maybeRegimeId         <- (json \ "regimeId").validateOpt[RegimeId]
        maybeServiceId        <- (json \ "serviceId").validateOpt[ServiceId]
        maybeAgentAccess      <- (json \ "agentAccess").validateOpt[AgentAccess]
        maybeEnrolmentSection <- (json \ "enrolmentSection").validateOpt[EnrolmentSection]
        maybeEnrolmentCheck   <- (json \ "enrolmentCheck").validateOpt[EnrolmentCheckVerb]
        authConfig <- authModule match {
                       case EeittLegacy =>
                         maybeRegimeId match {
                           case None =>
                             JsError(s"Missing regimeId (regimeId is mandatory for legacyEEITTAuth)")
                           case Some(regimeId) => JsSuccess(EeittModule(regimeId))
                         }
                       case Hmrc =>
                         maybeServiceId match {
                           case None =>
                             maybeAgentAccess.fold(JsSuccess(HmrcSimpleModule: AuthConfig))(agentAccess =>
                               JsSuccess(HmrcAgentModule(agentAccess)))
                           case Some(serviceId) =>
                             toEnrolmentAuth(serviceId, maybeRegimeId, maybeEnrolmentCheck, maybeEnrolmentSection) match {
                               case Left(error) => JsError(error)
                               case Right(enrolmentAuth) =>
                                 JsSuccess(
                                   maybeAgentAccess.fold(HmrcEnrolmentModule(enrolmentAuth): AuthConfig)(
                                     HmrcAgentWithEnrolmentModule(_, enrolmentAuth)))
                             }
                         }
                     }
      } yield authConfig
    }

    val writes: OWrites[AuthConfig] = derived.owrites
    val reads: Reads[AuthConfig] = derived.reads

    OFormat(reads | rawTemplateReads, writes)
  }
}

case class ServiceId(value: String) extends AnyVal
object ServiceId {
  implicit val format: OFormat[ServiceId] = ValueClassFormat.oformat("value", ServiceId.apply, _.value)
}

case class RegimeId(value: String) extends AnyVal
object RegimeId {
  implicit val format: Format[RegimeId] = ValueClassFormat.oformat("regimeId", RegimeId.apply, _.value)
}

sealed trait AgentAccess
case object RequireMTDAgentEnrolment extends AgentAccess
case object DenyAnyAgentAffinityUser extends AgentAccess
case object AllowAnyAgentAffinityUser extends AgentAccess
object AgentAccess {
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
