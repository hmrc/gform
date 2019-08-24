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

package uk.gov.hmrc.gform.submission

import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.{ FOpt, success }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.submission.generators.DestinationAuditGen
import uk.gov.hmrc.http.HeaderCarrier

class RepoDestinationAuditerSpec extends Spec with DestinationAuditGen {
  implicit val hc = HeaderCarrier()

  "getLatestForForm" should "returns an error when there are no audits for the form ID" in {
    val fixture = createAuditer

    val formId = FormId("theForm")

    fixture
      .expectAuditRepositoryFormIdSearch(formId, Nil)
      .auditer
      .getLatestForForm(formId)
      .value
      .futureValue
      .left
      .value shouldBe UnexpectedState(s"Could not find any audits for form with ID ${formId.value}")
  }

  "getLatestForForm" should "return the audit if there is only one" in {
    forAll(destinationAuditGen) { audit =>
      val fixture = createAuditer

      fixture
        .expectAuditRepositoryFormIdSearch(audit.formId, List(audit))
        .auditer
        .getLatestForForm(audit.formId)
        .value
        .futureValue
        .right
        .value shouldBe audit
    }
  }

  "getLatestForForm" should "returns the latest audit if there is more than one" in {
    forAll(destinationAuditGen, destinationAuditGen) {
      case (audit1: DestinationAudit, generatedAudit2) =>
        val audit2 = generatedAudit2.copy(formId = audit1.formId, timestamp = audit1.timestamp.plusSeconds(1))

        val fixture = createAuditer

        fixture
          .expectAuditRepositoryFormIdSearch(audit1.formId, List(audit1, audit2))
          .auditer
          .getLatestForForm(audit1.formId)
          .value
          .futureValue
          .right
          .value shouldBe audit2
    }
  }

  case class Fixture(auditer: RepoDestinationAuditer, auditRepository: RepoAlgebra[DestinationAudit, FOpt]) {
    def expectAuditRepositoryFormIdSearch(formId: FormId, result: List[DestinationAudit]): Fixture = {
      (auditRepository
        .search(_: JsObject))
        .expects(DestinationAuditAlgebra.auditRepoFormIdSearch(formId))
        .returning(success(result))

      this
    }
  }

  def createAuditer: Fixture = {
    val auditRepository = mock[RepoAlgebra[DestinationAudit, FOpt]]
    val auditer = new RepoDestinationAuditer(auditRepository, null, null)

    Fixture(auditer, auditRepository)
  }
}
