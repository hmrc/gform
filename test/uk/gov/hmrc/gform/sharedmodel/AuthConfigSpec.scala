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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class AuthConfigSpec extends Spec {

  it should "parse legacy EEITT auth" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "legacyEEITTAuth",
     |  "regimeId": "IP"
     |}""")
    authConfigValue shouldBe JsSuccess(
      EeittModule(RegimeId("IP"))
    )
  }

  it should "parse simplest HMRC auth" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc"
     |}""")
    authConfigValue shouldBe JsSuccess(HmrcSimpleModule)
  }

  it should "parse HMRC auth with agentAccess" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser"
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentModule(AllowAnyAgentAffinityUser)
    )
  }

  it should "parse HMRC auth with serviceId" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser",
     |  "enrolmentCheck": "always",
     |  "serviceId": "Z"
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(ServiceId("Z"), DoCheck(Always, RejectAccess, NoCheck)))
    )
  }

  it should "parse HMRC auth with regimeId" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser",
     |  "enrolmentCheck": "always",
     |  "serviceId": "Z",
     |  "regimeId": "IP"
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(ServiceId("Z"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("IP")))))
    )
  }

  it should "parse HMRC auth with enrolmentSection" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser",
     |  "enrolmentCheck": "always",
     |  "serviceId": "Z",
     |  "enrolmentSection": {"title": "t", "fields":[]}
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(ServiceId("Z"), DoCheck(Always, RequireEnrolment(EnrolmentSection("t", None, List())), NoCheck)))
    )
  }

  it should "parse HMRC auth with everything" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser",
     |  "serviceId": "Z",
     |  "enrolmentCheck": "always",
     |  "regimeId": "IP",
     |  "enrolmentSection": {"title": "t", "fields":[]}
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(
          ServiceId("Z"),
          DoCheck(Always, RequireEnrolment(EnrolmentSection("t", None, List())), RegimeIdCheck(RegimeId("IP"))))
      )
    )
  }

  private def toAuthConfig(authConfig: String): JsResult[AuthConfig] = {

    val authConfigAsJson = Json.parse(authConfig.stripMargin)

    implicitly[Reads[AuthConfig]].reads(authConfigAsJson)
  }
}
