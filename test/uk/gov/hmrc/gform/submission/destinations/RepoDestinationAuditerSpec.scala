/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.submission.destinations

import cats.syntax.eq._
import org.scalatest.time.{ Millis, Span }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.JsObject
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.core.{ FOpt, success }
import uk.gov.hmrc.gform.exceptions.UnexpectedState
import uk.gov.hmrc.gform.repo.RepoAlgebra
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SubmissionRefGen
import uk.gov.hmrc.http.HeaderCarrier

class RepoDestinationAuditerSpec
    extends Spec with DestinationAuditGen with SubmissionRefGen with ScalaCheckDrivenPropertyChecks {
  implicit val pc = patienceConfig.copy(timeout = scaled(Span(500, Millis)), interval = scaled(Span(50, Millis)))

  private implicit val hc = HeaderCarrier()

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

  it should "return the audit if there is only one" in {
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

  it should "return the latest audit if there is more than one" in {
    forAll(destinationAuditGen, destinationAuditGen, destinationAuditGen) {
      case (audit1: DestinationAudit, generatedAudit2, generatedAudit3) =>
        val audit2 = generatedAudit2.copy(formId = audit1.formId, timestamp = audit1.timestamp.plusSeconds(1))
        val audit3 = generatedAudit3.copy(formId = audit1.formId, timestamp = audit1.timestamp.minusSeconds(1))

        createAuditer
          .expectAuditRepositoryFormIdSearch(audit1.formId, List(audit2, audit1, audit3))
          .auditer
          .getLatestForForm(audit1.formId)
          .value
          .futureValue
          .right
          .value shouldBe audit2
    }
  }

  "findLatestChildAudits" should "return an empty list if no child audits can be found" in {
    val submissionRef = SubmissionRef("abc")

    createAuditer
      .expectAuditRepositoryFindLatestChildAudits(submissionRef, Nil)
      .auditer
      .findLatestChildAudits(submissionRef)
      .value
      .futureValue
      .right
      .value shouldBe Nil
  }

  it should "return all child audits that can be found when there is only one audit for each child" in {
    forAll(submissionRefGen, destinationAuditGen, destinationAuditGen) {
      case (parentSubmissionRef, generatedAudit1, generatedAudit2) =>
        whenever(generatedAudit1.formId =!= generatedAudit2.formId) {
          val audit1 = generatedAudit1.copy(parentFormSubmissionRefs = List(parentSubmissionRef.value))
          val audit2 = generatedAudit2.copy(parentFormSubmissionRefs = List(parentSubmissionRef.value))

          createAuditer
            .expectAuditRepositoryFindLatestChildAudits(parentSubmissionRef, List(audit1, audit2))
            .auditer
            .findLatestChildAudits(parentSubmissionRef)
            .value
            .futureValue
            .right
            .value
            .toSet shouldBe Set(audit1, audit2)
        }
    }
  }

  it should "return the latest child audit for each formId" in {
    forAll(submissionRefGen, destinationAuditGen, destinationAuditGen, destinationAuditGen) {
      (parentSubmissionRef, generatedAudit1, generatedAudit2, generatedAudit3) =>
        whenever(generatedAudit3.formId =!= generatedAudit1.formId) {
          val audit1 = generatedAudit1.copy(parentFormSubmissionRefs = List(parentSubmissionRef.value))
          val audit2 = generatedAudit2.copy(
            formId = audit1.formId,
            parentFormSubmissionRefs = List(parentSubmissionRef.value),
            timestamp = audit1.timestamp.plusSeconds(1)
          )
          val audit3 = generatedAudit3.copy(
            parentFormSubmissionRefs = List(parentSubmissionRef.value),
            timestamp = audit1.timestamp.minusSeconds(1)
          )

          createAuditer
            .expectAuditRepositoryFindLatestChildAudits(parentSubmissionRef, List(audit3, audit2, audit1))
            .auditer
            .findLatestChildAudits(parentSubmissionRef)
            .value
            .futureValue
            .right
            .value
            .toSet shouldBe Set(audit2, audit3)
        }
    }
  }

  case class Fixture(auditer: RepoDestinationAuditer, auditRepository: RepoAlgebra[DestinationAudit, FOpt]) {
    def expectAuditRepositoryFormIdSearch(formId: FormId, result: List[DestinationAudit]): Fixture = {
      (auditRepository
        .search(_: JsObject, _: JsObject))
        .expects(DestinationAuditAlgebra.auditRepoFormIdSearch(formId), DestinationAuditAlgebra.latestTimestampFirst)
        .returning(success(result))

      this
    }

    def expectAuditRepositoryFindLatestChildAudits(
      submissionRef: SubmissionRef,
      result: List[DestinationAudit]
    ): Fixture = {
      (auditRepository
        .search(_: JsObject))
        .expects(DestinationAuditAlgebra.auditRepoLatestChildAuditsSearch(submissionRef))
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
