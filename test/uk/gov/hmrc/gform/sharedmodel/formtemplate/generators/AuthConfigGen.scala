/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait AuthConfigGen {
  def regimeIdGen: Gen[RegimeId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(RegimeId(_))

  def serviceIdGen: Gen[ServiceId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(ServiceId(_))

  def enrolmentCheckPredicateGen: Gen[EnrolmentCheckPredicate] = Gen.oneOf(Always, ForNonAgents)

  def requireEnrolmentGen: Gen[RequireEnrolment] = SectionGen.enrolmentSectionGen.map(RequireEnrolment)
  def needEnrolmentGen: Gen[NeedEnrolment] = Gen.oneOf(requireEnrolmentGen, Gen.const(RejectAccess))

  def regimeIdCheckGen: Gen[RegimeIdCheck] = regimeIdGen.map(RegimeIdCheck)
  def enrolmentPostCheckGen: Gen[EnrolmentPostCheck] = Gen.oneOf(Gen.const(NoCheck), regimeIdCheckGen)

  def enrolmentCheckVerbGen: Gen[EnrolmentCheckVerb] = Gen.oneOf(NeverVerb, AlwaysVerb, ForNonAgentsVerb)

  def authModuleGen: Gen[AuthModule] = Gen.oneOf(Hmrc, EeittLegacy)

  def doCheckGen: Gen[DoCheck] =
    for {
      predicate     <- enrolmentCheckPredicateGen
      needEnrolment <- needEnrolmentGen
      check         <- enrolmentPostCheckGen
    } yield DoCheck(predicate, needEnrolment, check)

  def enrolmentCheckGen: Gen[EnrolmentCheck] = Gen.oneOf(
    doCheckGen,
    Gen.const(Never)
  )
  def enrolmentAuthGen: Gen[EnrolmentAuth] =
    for {
      serviceId      <- serviceIdGen
      enrolmentCheck <- enrolmentCheckGen
    } yield EnrolmentAuth(serviceId, enrolmentCheck)

  def eeittModuleGen: Gen[EeittModule] = regimeIdGen.map(EeittModule)

  def hmrcEnrolmentModuleGen: Gen[HmrcEnrolmentModule] = enrolmentAuthGen.map(HmrcEnrolmentModule)

  def agentAccessGen: Gen[AgentAccess] =
    Gen.oneOf(RequireMTDAgentEnrolment, DenyAnyAgentAffinityUser, AllowAnyAgentAffinityUser)

  def hmrcAgentModuleGen: Gen[HmrcAgentModule] = agentAccessGen.map(HmrcAgentModule)

  def hmrcAgentWithEnrolmentModuleGen: Gen[HmrcAgentWithEnrolmentModule] =
    for {
      agentAccess   <- agentAccessGen
      enrolmentAuth <- enrolmentAuthGen
    } yield HmrcAgentWithEnrolmentModule(agentAccess, enrolmentAuth)

  def anonymousGen = Gen.const(Anonymous)

  def authConfigGen: Gen[AuthConfig] =
    Gen.oneOf(
      eeittModuleGen,
      Gen.const(HmrcSimpleModule),
      hmrcEnrolmentModuleGen,
      hmrcAgentModuleGen,
      hmrcAgentWithEnrolmentModuleGen)
}

object AuthConfigGen extends AuthConfigGen
