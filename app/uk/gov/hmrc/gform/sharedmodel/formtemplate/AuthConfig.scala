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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import play.api.libs.functional.syntax._
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat
import uk.gov.hmrc.gform.sharedmodel.{ EmailVerifierService, LocalisedString, ValueClassFormat }

case class EnrolmentAuth(
  serviceId: ServiceId,
  enrolmentCheck: EnrolmentCheck,
  enrolmentOutcomes: EnrolmentOutcomes
)
object EnrolmentAuth {
  implicit val format: OFormat[EnrolmentAuth] = derived.oformat()
}

sealed trait EnrolmentCheck extends Product with Serializable
case class DoCheck(
  enrolmentCheckPredicate: EnrolmentCheckPredicate,
  needEnrolment: NeedEnrolment,
  check: EnrolmentPostCheck
) extends EnrolmentCheck
case object Never extends EnrolmentCheck
object EnrolmentCheck {
  implicit val format: OFormat[EnrolmentCheck] = derived.oformat()
}

sealed trait EnrolmentCheckPredicate extends Product with Serializable
case object Always extends EnrolmentCheckPredicate
case object ForNonAgents extends EnrolmentCheckPredicate
object EnrolmentCheckPredicate {
  implicit val format: OFormat[EnrolmentCheckPredicate] = derived.oformat()
}

sealed trait NeedEnrolment extends Product with Serializable
case class RequireEnrolment(enrolmentSection: EnrolmentSection, enrolmentAction: EnrolmentAction) extends NeedEnrolment
case object RejectAccess extends NeedEnrolment
object NeedEnrolment {
  implicit val format: OFormat[NeedEnrolment] = derived.oformat()
}

sealed trait EnrolmentPostCheck extends Product with Serializable
case object NoCheck extends EnrolmentPostCheck
case class RegimeIdCheck(regimeId: RegimeId) extends EnrolmentPostCheck
object EnrolmentPostCheck {
  implicit val format: OFormat[EnrolmentPostCheck] = derived.oformat()
}

sealed trait EnrolmentCheckVerb extends Product with Serializable
case object NeverVerb extends EnrolmentCheckVerb
case object AlwaysVerb extends EnrolmentCheckVerb
case object ForNonAgentsVerb extends EnrolmentCheckVerb

object EnrolmentCheckVerb {

  private val never = "never"
  private val always = "always"
  private val forNonAgents = "forNonAgents"

  implicit val format: Format[EnrolmentCheckVerb] =
    ADTFormat.formatEnumeration(never -> NeverVerb, always -> AlwaysVerb, forNonAgents -> ForNonAgentsVerb)

  def asString(verb: EnrolmentCheckVerb): String = verb match {
    case NeverVerb        => never
    case AlwaysVerb       => always
    case ForNonAgentsVerb => forNonAgents
  }
}

sealed trait MinimumConfidenceLevel extends Product with Serializable
case object CL200 extends MinimumConfidenceLevel
case object CL250 extends MinimumConfidenceLevel

object MinimumConfidenceLevel {

  private val cl200 = "200"
  private val cl250 = "250"

  implicit val format: Format[MinimumConfidenceLevel] =
    ADTFormat.formatEnumeration(cl200 -> CL200, cl250 -> CL250)

  def asString(cl: MinimumConfidenceLevel): String = cl match {
    case CL200 => cl200
    case CL250 => cl250
  }
}

sealed trait AuthModule extends Product with Serializable

object AuthModule {

  case object Hmrc extends AuthModule
  case object Email extends AuthModule
  case object HmrcAny extends AuthModule
  case object HmrcVerified extends AuthModule
  case object AnonymousAccess extends AuthModule
  case object AWSALBAccess extends AuthModule
  case object OfstedModule extends AuthModule
  case object Composite extends AuthModule

  private val hmrc = "hmrc"
  private val email = "email"
  private val hmrcAny = "hmrcAny"
  private val hmrcVerified = "hmrcVerified"
  private val anonymous = "anonymous"
  private val awsAlb = "awsAlbAuth"
  private val ofstedAuth = "ofsted"
  private val composite = "composite"

  implicit val format: Format[AuthModule] = ADTFormat.formatEnumeration(
    hmrc         -> Hmrc,
    email        -> Email,
    hmrcAny      -> HmrcAny,
    hmrcVerified -> HmrcVerified,
    anonymous    -> AnonymousAccess,
    awsAlb       -> AWSALBAccess,
    ofstedAuth   -> OfstedModule,
    composite    -> Composite
  )

  def asString(o: AuthModule): String = o match {
    case Hmrc            => hmrc
    case Email           => email
    case HmrcAny         => hmrcAny
    case HmrcVerified    => hmrcVerified
    case AnonymousAccess => anonymous
    case AWSALBAccess    => awsAlb
    case OfstedModule    => ofstedAuth
    case Composite       => composite
  }
}

sealed trait AuthConfig extends Product with Serializable
case object Anonymous extends AuthConfig
case object AWSALBAuth extends AuthConfig
case class EmailAuthConfig(
  service: EmailVerifierService,
  emailUseInfo: Option[LocalisedString],
  emailCodeHelp: Option[LocalisedString],
  emailConfirmation: Option[LocalisedString]
) extends AuthConfig
case object HmrcAny extends AuthConfig
case class HmrcVerified(
  ivFailure: LocalisedString,
  agentAccess: AgentAccess,
  minimumCL: String,
  allowOrganisations: Boolean,
  allowSAIndividuals: Boolean
) extends AuthConfig
case object HmrcSimpleModule extends AuthConfig
case class HmrcEnrolmentModule(enrolmentAuth: EnrolmentAuth) extends AuthConfig
case class HmrcAgentModule(agentAccess: AgentAccess) extends AuthConfig
case class HmrcAgentWithEnrolmentModule(agentAccess: AgentAccess, enrolmentAuth: EnrolmentAuth) extends AuthConfig
case object OfstedUser extends AuthConfig
case class Composite(configs: NonEmptyList[AuthConfig]) extends AuthConfig

object HasEnrolmentSection {
  def unapply(ac: AuthConfig): Option[(ServiceId, EnrolmentSection, EnrolmentAction)] = ac match {
    case HmrcEnrolmentModule(EnrolmentAuth(serviceId, DoCheck(_, RequireEnrolment(es, enrolmentAction), _), _)) =>
      Some((serviceId, es, enrolmentAction))
    case HmrcAgentWithEnrolmentModule(
          _,
          EnrolmentAuth(serviceId, DoCheck(_, RequireEnrolment(es, enrolmentAction), _), _)
        ) =>
      Some((serviceId, es, enrolmentAction))
    case _ => None
  }
}

object AuthConfig {

  private def toEnrolmentPostCheck(maybeRegimeId: Option[RegimeId]): EnrolmentPostCheck =
    maybeRegimeId.fold(NoCheck: EnrolmentPostCheck)(RegimeIdCheck.apply)

  def enrolmentActionMatch(enrolmentAction: Option[EnrolmentAction]): EnrolmentAction =
    enrolmentAction.getOrElse(NoAction)

  private def toEnrolmentAuth(
    serviceId: ServiceId,
    maybeRegimeId: Option[RegimeId],
    maybeEnrolmentAction: Option[EnrolmentAction],
    maybeEnrolmentCheck: Option[EnrolmentCheckVerb],
    maybeEnrolmentSection: Option[EnrolmentSection],
    enrolmentOutcomes: EnrolmentOutcomes
  ): EnrolmentAuth =
    (maybeEnrolmentCheck, maybeEnrolmentSection) match {
      case (Some(AlwaysVerb), Some(enrolmentSection)) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(
            Always,
            RequireEnrolment(enrolmentSection, enrolmentActionMatch(maybeEnrolmentAction)),
            toEnrolmentPostCheck(maybeRegimeId)
          ),
          enrolmentOutcomes
        )
      case (Some(ForNonAgentsVerb), Some(enrolmentSection)) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(
            ForNonAgents,
            RequireEnrolment(enrolmentSection, enrolmentActionMatch(maybeEnrolmentAction)),
            toEnrolmentPostCheck(maybeRegimeId)
          ),
          enrolmentOutcomes
        )
      case (Some(AlwaysVerb), None) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(Always, RejectAccess, toEnrolmentPostCheck(maybeRegimeId)),
          enrolmentOutcomes
        )
      case (Some(ForNonAgentsVerb), None) =>
        EnrolmentAuth(
          serviceId,
          DoCheck(ForNonAgents, RejectAccess, toEnrolmentPostCheck(maybeRegimeId)),
          enrolmentOutcomes
        )
      case (Some(NeverVerb) | None, _) => EnrolmentAuth(serviceId, Never, enrolmentOutcomes)
    }

  implicit val format: OFormat[AuthConfig] = {
    val rawTemplateReads = Reads[AuthConfig] { json =>
      for {
        authModule                     <- (json \ "authModule").validate[AuthModule]
        maybeRegimeId                  <- (json \ "regimeId").validateOpt[RegimeId]
        maybeServiceId                 <- (json \ "serviceId").validateOpt[ServiceId]
        maybeLegacyFcEnrolmentVerifier <- (json \ "legacyFcEnrolmentVerifier").validateOpt[LegacyFcEnrolmentVerifier]
        maybeAgentAccess               <- (json \ "agentAccess").validateOpt[AgentAccess]
        maybeEnrolmentSection          <- (json \ "enrolmentSection").validateOpt[EnrolmentSection]
        maybeEnrolmentCheck            <- (json \ "enrolmentCheck").validateOpt[EnrolmentCheckVerb]
        maybeIvFailure                 <- (json \ "ivFailure").validateOpt[LocalisedString]
        maybeMinimumCL                 <- (json \ "minimumCL").validateOpt[MinimumConfidenceLevel]
        maybeEmailCodeTemplate         <- (json \ "emailCodeTemplate").validateOpt[LocalisedEmailTemplateId]
        maybeEmailUseInfo              <- (json \ "emailUseInfo").validateOpt[LocalisedString]
        maybeEmailCodeHelp             <- (json \ "emailCodeHelp").validateOpt[LocalisedString]
        maybeEmailConfirmation         <- (json \ "emailConfirmation").validateOpt[LocalisedString]
        maybeEmailService              <- (json \ "emailService").validateOpt[String]
        maybeCompositeConfigs          <- (json \ "configs").validateOpt[NonEmptyList[AuthConfig]]
        maybeEnrolmentOutcomes         <- (json \ "enrolmentOutcomes").validateOpt[EnrolmentOutcomes]
        maybeAllowOrganisations        <- (json \ "allowOrganisations").validateOpt[Boolean]
        maybeAllowSAIndividuals        <- (json \ "allowSAIndividuals").validateOpt[Boolean]

        authConfig <- authModule match {
                        case AuthModule.AnonymousAccess => JsSuccess(Anonymous)
                        case AuthModule.AWSALBAccess    => JsSuccess(AWSALBAuth)
                        case AuthModule.HmrcAny         => JsSuccess(HmrcAny)
                        case AuthModule.HmrcVerified =>
                          (
                            maybeIvFailure,
                            maybeAgentAccess,
                            maybeMinimumCL,
                            maybeAllowOrganisations,
                            maybeAllowSAIndividuals
                          ) match {
                            case (
                                  Some(ivFailure),
                                  Some(maybeAgentAccess),
                                  Some(maybeMinimumCL),
                                  Some(allowOrganisations),
                                  Some(allowSAIndividuals)
                                ) =>
                              JsSuccess(
                                HmrcVerified(
                                  ivFailure,
                                  maybeAgentAccess,
                                  MinimumConfidenceLevel.asString(maybeMinimumCL),
                                  allowOrganisations,
                                  allowSAIndividuals
                                )
                              )
                            case (
                                  otherIvFailure,
                                  otherAgentAccess,
                                  otherMinimumCL,
                                  otherAllowOrganisations,
                                  otherAllowSAIndividuals
                                ) =>
                              JsError(
                                s"Missing ${otherIvFailure.map(_ => "").getOrElse("ivFailure ")}" +
                                  s"${otherAgentAccess.map(_ => "").getOrElse("agentAccess ")}" +
                                  s"${otherMinimumCL.map(_ => "").getOrElse("minimumCL ")}" +
                                  s"${otherAllowOrganisations.map(_ => "").getOrElse("allowOrganisations ")}" +
                                  s"${otherAllowSAIndividuals.map(_ => "").getOrElse("allowSAIndividuals ")}field"
                              )
                          }
                        case AuthModule.Hmrc =>
                          maybeServiceId match {
                            case None =>
                              maybeAgentAccess.fold(JsSuccess(HmrcSimpleModule: AuthConfig))(agentAccess =>
                                JsSuccess(HmrcAgentModule(agentAccess))
                              )
                            case Some(serviceId) =>
                              maybeEnrolmentOutcomes match {
                                case Some(enrolmentOutcomes) =>
                                  val enrolmentAuth =
                                    toEnrolmentAuth(
                                      serviceId,
                                      maybeRegimeId,
                                      maybeLegacyFcEnrolmentVerifier,
                                      maybeEnrolmentCheck,
                                      maybeEnrolmentSection,
                                      enrolmentOutcomes
                                    )

                                  JsSuccess(
                                    maybeAgentAccess.fold(HmrcEnrolmentModule(enrolmentAuth): AuthConfig)(
                                      HmrcAgentWithEnrolmentModule(_, enrolmentAuth)
                                    )
                                  )
                                case None =>
                                  JsError(
                                    "Error: Missing required field 'enrolmentOutcomes'. When 'enrolmentSection' is defined it must contain field name 'enrolmentOutcomes'"
                                  )
                              }
                          }
                        case AuthModule.Email =>
                          maybeEmailCodeTemplate match {
                            case Some(localisedEmailTemplateId) =>
                              maybeEmailService match {
                                case Some("dc") | None =>
                                  JsSuccess(
                                    EmailAuthConfig(
                                      localisedEmailTemplateId.toDigitalContact,
                                      maybeEmailUseInfo,
                                      maybeEmailCodeHelp,
                                      maybeEmailConfirmation
                                    )
                                  )
                                case Some("notify") =>
                                  JsSuccess(
                                    EmailAuthConfig(
                                      localisedEmailTemplateId.toNotify,
                                      maybeEmailUseInfo,
                                      maybeEmailCodeHelp,
                                      maybeEmailConfirmation
                                    )
                                  )
                                case Some(other) => JsError(s"Invalid 'emailService' value for email auth $other")
                              }
                            case None => JsError("Missing 'emailCodeTemplate' field for email auth")
                          }
                        case AuthModule.Composite =>
                          maybeCompositeConfigs match {
                            case Some(configs) =>
                              val notAllowedConfigs = configs.toList.collectFirst {
                                case v @ (Anonymous | AWSALBAuth | HmrcAny | HmrcVerified(_, _, _, _, _) |
                                    HmrcEnrolmentModule(_) | HmrcAgentModule(_) | HmrcAgentWithEnrolmentModule(_, _) |
                                    OfstedUser | Composite(_)) =>
                                  v
                              }
                              if (notAllowedConfigs.isEmpty)
                                JsSuccess(Composite(configs))
                              else
                                JsError("Only hmrc and email auths are allowed inside composite auth")

                            case None => JsError("Missing 'configs' field for composite auth")
                          }
                        case AuthModule.OfstedModule => JsSuccess(OfstedUser)
                      }
      } yield authConfig
    }

    val writes: OWrites[AuthConfig] = derived.owrites()
    val reads: Reads[AuthConfig] = derived.reads()

    OFormat(reads | rawTemplateReads, writes)
  }
}

case class ServiceId(value: String) extends AnyVal
object ServiceId {
  implicit val format: OFormat[ServiceId] = ValueClassFormat.oformat("value", ServiceId.apply, _.value)
}

sealed trait EnrolmentAction
case class LegacyFcEnrolmentVerifier(value: String) extends EnrolmentAction
case object NoAction extends EnrolmentAction

object EnrolmentAction {
  implicit val format: Format[EnrolmentAction] = derived.oformat()
}

object LegacyFcEnrolmentVerifier {
  implicit val format: Format[LegacyFcEnrolmentVerifier] =
    ValueClassFormat.oformat("value", LegacyFcEnrolmentVerifier.apply, _.value)

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
  private val requireMTDAgentEnrolment = "requireMTDAgentEnrolment"
  private val denyAnyAgentAffinityUser = "denyAnyAgentAffinityUser"
  private val allowAnyAgentAffinityUser = "allowAnyAgentAffinityUser"

  implicit val format: Format[AgentAccess] = ADTFormat.formatEnumerationWithDefault(
    RequireMTDAgentEnrolment,
    Seq(RequireMTDAgentEnrolment, DenyAnyAgentAffinityUser, AllowAnyAgentAffinityUser).map(t => (asString(t) -> t)): _*
  )

  def asString(access: AgentAccess): String = access match {
    case RequireMTDAgentEnrolment  => requireMTDAgentEnrolment
    case DenyAnyAgentAffinityUser  => denyAnyAgentAffinityUser
    case AllowAnyAgentAffinityUser => allowAnyAgentAffinityUser
  }
}
