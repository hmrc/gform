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

package uk.gov.hmrc.gform.services

import java.time.LocalDateTime

import org.scalatestplus.play.PlaySpec
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormKind, FormTemplateId }
import uk.gov.hmrc.gform.form.LifeCycleStatus

class FormServiceSpec extends PlaySpec {

  val testFormInProgress = Form(
    FormId("testId"),
    EnvelopeId("env-id"),
    UserId("testUser"),
    FormTemplateId("temp-id"),
    None,
    FormData(Seq(FormField(FormComponentId("a"), "1"))),
    InProgress,
    VisitIndex.empty(FormKind.Classic(Nil)),
    ThirdPartyData.empty,
    Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1))),
    FormComponentIdToFileIdMapping.empty,
    TaskIdTaskStatusMapping.empty,
    ConfirmationExprMapping.empty
  )

  val testFormSummary: Form = testFormInProgress.copy(status = Summary)
  val testFormValidated: Form = testFormInProgress.copy(status = Validated)
  val testFormSigned: Form = testFormInProgress.copy(status = Signed)

  "The form status " must {

    "change from InProgress to Summary" in {
      LifeCycleStatus.newStatus(testFormInProgress, Summary) mustBe Summary
    }

    "be demoted to Summary after being in Validated status and the user has updated form data" in {
      LifeCycleStatus.newStatus(testFormValidated, InProgress) mustBe Summary
    }
  }
}
