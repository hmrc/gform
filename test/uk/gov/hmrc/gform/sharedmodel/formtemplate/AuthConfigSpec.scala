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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{ JsError, JsResult, JsSuccess, Json, JsonValidationError, Reads }
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService.{ DigitalContact, Notify }
import uk.gov.hmrc.gform.sharedmodel.email.EmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.AuthConfigGen
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierTemplateId

class AuthConfigSpec extends Spec with ScalaCheckDrivenPropertyChecks {

  val enrolmentOutcome = EnrolmentOutcome(toSmartString("title"), toSmartString("content"))
  val enrolmentOutcomes =
    EnrolmentOutcomes(enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome)

  "Default Read and Write" should "round trip derived JSON" in {
    forAll(AuthConfigGen.authConfigGen) { c =>
      verifyRoundTrip(c)
    }
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
    val authConfigValue = toAuthConfig("""
      |{
      |  "authModule": "hmrc",
      |  "agentAccess": "allowAnyAgentAffinityUser",
      |  "enrolmentCheck": "always",
      |  "serviceId": "Z",
      |  "enrolmentOutcomes": {
      |    "notMatchedPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "alreadyLinkedPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "technicalFailurePage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "successPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "insufficientCredentialsPage": {
      |      "title": "title",
      |      "content": "content"
      |    }
      |  }
      |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(
          ServiceId("Z"),
          DoCheck(Always, RejectAccess, NoCheck),
          enrolmentOutcomes
        )
      )
    )
  }

  it should "parse HMRC auth with regimeId" in {
    val authConfigValue = toAuthConfig(s"""|{
     |  "authModule": "hmrc",
     |  "agentAccess": "allowAnyAgentAffinityUser",
     |  "enrolmentCheck": "always",
     |  "serviceId": "Z",
     |  "regimeId": "IP",
     |  "enrolmentOutcomes": {
     |    "notMatchedPage": {
     |      "title": "title",
     |      "content": "content"
     |    },
     |    "alreadyLinkedPage": {
     |      "title": "title",
     |      "content": "content"
     |    },
     |    "technicalFailurePage": {
     |      "title": "title",
     |      "content": "content"
     |    },
     |    "successPage": {
     |      "title": "title",
     |      "content": "content"
     |    },
     |    "insufficientCredentialsPage": {
     |      "title": "title",
     |      "content": "content"
     |    }
     |  }
     |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(ServiceId("Z"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("IP"))), enrolmentOutcomes)
      )
    )
  }

  it should "parse HMRC auth with enrolmentSection" in {
    val authConfigValue = toAuthConfig("""
      |{
      |  "authModule": "hmrc",
      |  "agentAccess": "allowAnyAgentAffinityUser",
      |  "enrolmentCheck": "always",
      |  "serviceId": "Z",
      |  "enrolmentSection": {
      |     "title": "t",
      |     "fields":[],
      |     "identifiers": [
      |        {
      |          "key": "EtmpRegistrationNumber",
      |          "value": "${eeittReferenceNumber}"
      |        }
      |     ],
      |     "verifiers": []
      |   },
      |  "enrolmentOutcomes": {
      |    "notMatchedPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "alreadyLinkedPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "technicalFailurePage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "successPage": {
      |      "title": "title",
      |      "content": "content"
      |    },
      |    "insufficientCredentialsPage": {
      |      "title": "title",
      |      "content": "content"
      |    }
      |  }
      |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(
          ServiceId("Z"),
          DoCheck(
            Always,
            RequireEnrolment(
              EnrolmentSection(
                toSmartString("t"),
                None,
                None,
                List.empty,
                NonEmptyList.of(
                  IdentifierRecipe("EtmpRegistrationNumber", FormCtx(FormComponentId("eeittReferenceNumber")))
                ),
                List.empty,
                None
              ),
              NoAction
            ),
            NoCheck
          ),
          enrolmentOutcomes
        )
      )
    )
  }

  it should "parse HMRC auth with everything" in {
    val authConfigValue = toAuthConfig("""
    |{
    |  "authModule": "hmrc",
    |  "agentAccess": "allowAnyAgentAffinityUser",
    |  "serviceId": "Z",
    |  "enrolmentCheck": "always",
    |  "regimeId": "IP",
    |  "legacyFcEnrolmentVerifier": "NonUKCountryCode",
    |  "enrolmentSection": {
    |     "title": "t",
    |     "fields":[],
    |     "identifiers": [
    |        {
    |          "key": "EtmpRegistrationNumber",
    |          "value": "${eeittReferenceNumber}"
    |        }
    |     ],
    |     "verifiers": [],
    |     "continueLabel": "Save"
    |   },
    |  "enrolmentOutcomes": {
    |    "notMatchedPage": {
    |      "title": "title",
    |      "content": "content"
    |    },
    |    "alreadyLinkedPage": {
    |      "title": "title",
    |      "content": "content"
    |    },
    |    "technicalFailurePage": {
    |      "title": "title",
    |      "content": "content"
    |    },
    |    "successPage": {
    |      "title": "title",
    |      "content": "content"
    |    },
    |    "insufficientCredentialsPage": {
    |      "title": "title",
    |      "content": "content"
    |    }
    |  }
    |}""")
    authConfigValue shouldBe JsSuccess(
      HmrcAgentWithEnrolmentModule(
        AllowAnyAgentAffinityUser,
        EnrolmentAuth(
          ServiceId("Z"),
          DoCheck(
            Always,
            RequireEnrolment(
              EnrolmentSection(
                toSmartString("t"),
                None,
                None,
                List.empty,
                NonEmptyList.of(
                  IdentifierRecipe("EtmpRegistrationNumber", FormCtx(FormComponentId("eeittReferenceNumber")))
                ),
                List.empty,
                Some(toSmartString("Save"))
              ),
              LegacyFcEnrolmentVerifier("NonUKCountryCode")
            ),
            RegimeIdCheck(RegimeId("IP"))
          ),
          enrolmentOutcomes
        )
      )
    )
  }

  it should "parse email auth with no 'emailService'" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(DigitalContact(EmailTemplateId("someTemplate"), None), None, None, None)
    )
  }

  it should "parse email auth with no 'emailService' having localised emailCodeTemplate" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |      "en": "someTemplate-en",
                                           |      "cy": "someTemplate-cy"
                                           |    }
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        DigitalContact(EmailTemplateId("someTemplate-en"), Some(EmailTemplateId("someTemplate-cy"))),
        None,
        None,
        None
      )
    )
  }

  it should "parse email auth with no 'emailService' but having optional EmailAuthConfig details" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailUseInfo": "useInfo",
                                           |  "emailCodeHelp": "codeHelp",
                                           |  "emailConfirmation": "confirmation"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        DigitalContact(EmailTemplateId("someTemplate"), None),
        Some(toLocalisedString("useInfo")),
        Some(toLocalisedString("codeHelp")),
        Some(toLocalisedString("confirmation"))
      )
    )
  }

  it should "parse email auth with 'emailService' (dc)" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailService": "dc"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(DigitalContact(EmailTemplateId("someTemplate"), None), None, None, None)
    )
  }

  it should "parse email auth with 'emailService' (dc) having optional EmailAuthConfig details" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailService": "dc",
                                           |  "emailUseInfo": "useInfo",
                                           |  "emailCodeHelp": "codeHelp",
                                           |  "emailConfirmation": "confirmation"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        DigitalContact(EmailTemplateId("someTemplate"), None),
        Some(toLocalisedString("useInfo")),
        Some(toLocalisedString("codeHelp")),
        Some(toLocalisedString("confirmation"))
      )
    )
  }

  it should "parse email auth with 'emailService' (dc) having localised emailCodeTemplate" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |      "en": "someTemplate-En",
                                           |      "cy": "someTemplate-Cy"
                                           |    },
                                           |  "emailService": "dc"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        DigitalContact(EmailTemplateId("someTemplate-En"), Some(EmailTemplateId("someTemplate-Cy"))),
        None,
        None,
        None
      )
    )
  }

  it should "parse email auth with 'emailService' (dc) having localised emailCodeTemplate with only english template version" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |      "en": "someTemplate-En"
                                           |    },
                                           |  "emailService": "dc"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(DigitalContact(EmailTemplateId("someTemplate-En"), None), None, None, None)
    )
  }

  it should "parse email auth with 'emailService' (dc) having localised emailCodeTemplate with only welsh template version" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |      "cy": "someTemplate-Cy"
                                           |    },
                                           |  "emailService": "dc"
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Invalid email template id definition. Missing 'en' field with english template id")
    )
  }

  it should "return error with 'emailService' (dc) having emailCodeTemplate of wrong type (not string or object)" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": true,
                                           |  "emailService": "dc"
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Invalid email template id definition. Expected json String or json Object, but got: true")
    )
  }

  it should "parse email auth with 'emailService' (notify)" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailService": "notify"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(Notify(NotifierTemplateId("someTemplate"), None), None, None, None)
    )
  }

  it should "parse email auth with 'emailService' (notify) having optional EmailAuthConfig details" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailService": "notify",
                                           |  "emailUseInfo": "useInfo",
                                           |  "emailCodeHelp": "codeHelp",
                                           |  "emailConfirmation": "confirmation"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        Notify(NotifierTemplateId("someTemplate"), None),
        Some(toLocalisedString("useInfo")),
        Some(toLocalisedString("codeHelp")),
        Some(toLocalisedString("confirmation"))
      )
    )
  }

  it should "parse email auth with 'emailService' (notify) having localised emailCodeTemplate" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |    "en": "someTemplate",
                                           |    "cy": "someTemplateCy"
                                           |  },
                                           |  "emailService": "notify"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        Notify(NotifierTemplateId("someTemplate"), Some(NotifierTemplateId("someTemplateCy"))),
        None,
        None,
        None
      )
    )
  }

  it should "parse email auth with 'emailService' (notify) having localised emailCodeTemplate with only english template version" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |    "en": "someTemplate"
                                           |  },
                                           |  "emailService": "notify"
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      EmailAuthConfig(
        Notify(NotifierTemplateId("someTemplate"), None),
        None,
        None,
        None
      )
    )
  }

  it should "return error with 'emailService' (notify) having localised emailCodeTemplate with missing english template version" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": {
                                           |    "cy": "someTemplateCy"
                                           |  },
                                           |  "emailService": "notify"
                                           |}""".stripMargin)

    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Invalid email template id definition. Missing 'en' field with english template id")
    )
  }

  it should "return error with 'emailService' (notify) having emailCodeTemplate of wrong type (not string or object)" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": true,
                                           |  "emailService": "notify"
                                           |}""".stripMargin)

    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Invalid email template id definition. Expected json String or json Object, but got: true")
    )
  }

  it should "return error for email auth with invalid 'emailService'" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email",
                                           |  "emailCodeTemplate": "someTemplate",
                                           |  "emailService": "invalid"
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Invalid 'emailService' value for email auth invalid")
    )
  }

  it should "return error for email auth when 'emailCodeTemplate' missing" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "email"
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Missing 'emailCodeTemplate' field for email auth")
    )
  }

  it should "parse composite auth with `hmrc` and `email` auths" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "composite",
                                           |  "configs": [
                                           |    {
                                           |      "authModule": "hmrc"
                                           |    },
                                           |    {
                                           |      "authModule": "email",
                                           |      "emailCodeTemplate": {
                                           |        "en": "someTemplate-En",
                                           |        "cy": "someTemplate-Cy"
                                           |      }
                                           |    }
                                           |  ]
                                           |}""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      Composite(
        NonEmptyList.of(
          HmrcSimpleModule,
          EmailAuthConfig(
            DigitalContact(EmailTemplateId("someTemplate-En"), Some(EmailTemplateId("someTemplate-Cy"))),
            None,
            None,
            None
          )
        )
      )
    )
  }

  it should "return error for composite auth without `configs`" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "composite"
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Missing 'configs' field for composite auth")
    )
  }

  it should "return error for composite auth having auths other than hmrc and email auths`" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |  "authModule": "composite",
                                           |  "configs": [
                                           |    {
                                           |      "authModule": "hmrc"
                                           |    },
                                           |    {
                                           |      "authModule": "email",
                                           |      "emailCodeTemplate": {
                                           |        "en": "someTemplate-En",
                                           |        "cy": "someTemplate-Cy"
                                           |      }
                                           |    },
                                           |    {
                                           |      "authModule": "hmrcAny"
                                           |    }
                                           |  ]
                                           |}""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Only hmrc and email auths are allowed inside composite auth")
    )
  }

  it should "return error for hmrcVerified auth without allowOrganisations defined" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |    "authModule": "hmrcVerified",
                                           |    "ivFailure": "#You are unable to use this service because we have not been able to confirm your identity",
                                           |    "agentAccess": "denyAnyAgentAffinityUser",
                                           |    "minimumCL": "200",
                                           |    "allowSAIndividuals": false
                                           |  }""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Missing allowOrganisations field")
    )
  }

  it should "return error for hmrcVerified auth without allowSAIndividuals defined" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |    "authModule": "hmrcVerified",
                                           |    "ivFailure": "#You are unable to use this service because we have not been able to confirm your identity",
                                           |    "agentAccess": "denyAnyAgentAffinityUser",
                                           |    "minimumCL": "200",
                                           |    "allowOrganisations": false
                                           |  }""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError("Missing allowSAIndividuals field")
    )
  }

  it should "return error for hmrcVerified auth with minimumCL 50" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |    "authModule": "hmrcVerified",
                                           |    "ivFailure": "#You are unable to use this service because we have not been able to confirm your identity",
                                           |    "agentAccess": "denyAnyAgentAffinityUser",
                                           |    "minimumCL": "50",
                                           |    "allowOrganisations": false,
                                           |    "allowSAIndividuals": false
                                           |  }""".stripMargin)
    authConfigValue.isError shouldBe true
    authConfigValue.asInstanceOf[JsError].errors.flatMap(_._2) should contain(
      JsonValidationError(
        "Invalid JSON value (50) for custom read of uk.gov.hmrc.gform.sharedmodel.formtemplate.MinimumConfidenceLevel. allowed values are \"200\", \"250\"."
      )
    )
  }

  it should "parse hmrcVerified auth with everything" in {
    val authConfigValue = toAuthConfig(s"""|{
                                           |    "authModule": "hmrcVerified",
                                           |    "ivFailure": "#You are unable to use this service because we have not been able to confirm your identity",
                                           |    "agentAccess": "denyAnyAgentAffinityUser",
                                           |    "minimumCL": "200",
                                           |    "allowOrganisations": false,
                                           |    "allowSAIndividuals": true
                                           |  }""".stripMargin)
    authConfigValue shouldBe JsSuccess(
      HmrcVerified(
        toLocalisedString("#You are unable to use this service because we have not been able to confirm your identity"),
        DenyAnyAgentAffinityUser,
        "200",
        allowOrganisations = false,
        allowSAIndividuals = true
      )
    )
  }

  "enrolmentActionMatch" should "return no action with input None" in {
    AuthConfig.enrolmentActionMatch(None) shouldBe NoAction
  }

  it should "return no action with input NoAction" in {
    AuthConfig.enrolmentActionMatch(Some(NoAction)) shouldBe NoAction
  }

  it should "return no action with input LegacyFcEnrolmentVerifier('NonUKCountryCode')" in {
    AuthConfig.enrolmentActionMatch(
      Some(LegacyFcEnrolmentVerifier("NonUKCountryCode"))
    ) shouldBe LegacyFcEnrolmentVerifier("NonUKCountryCode")
  }

  private def toAuthConfig(authConfig: String): JsResult[AuthConfig] = {

    val authConfigAsJson = Json.parse(authConfig.stripMargin)

    implicitly[Reads[AuthConfig]].reads(authConfigAsJson)
  }
}
