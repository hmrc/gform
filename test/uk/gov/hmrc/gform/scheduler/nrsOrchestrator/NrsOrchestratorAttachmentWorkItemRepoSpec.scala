/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.scheduler.nrsOrchestrator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.crypto.{ Decrypter, Encrypter }
import uk.gov.hmrc.crypto.SymmetricCryptoFactory.{ aesCrypto, composeCrypto }
import uk.gov.hmrc.gform.nrs.{ BusinessId, NRSAttachment }
import uk.gov.hmrc.gform.save4later.EncryptedFormat
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

class NrsOrchestratorAttachmentWorkItemRepoSpec extends AnyFlatSpec with Matchers {
  private implicit val jsonCrypto: Encrypter with Decrypter = composeCrypto(
    aesCrypto("fqpLDZ4sumDsekHkeEBlCA=="),
    Seq()
  )
  "NrsOrchestratorAttachmentWorkItemRepoSpec" should "serialise and de-serialise into the same value" in {
    val obj = NrsOrchestratorAttachmentWorkItem(
      EnvelopeId("test"),
      FormTemplateId("test"),
      SubmissionRef("test"),
      NrsOrchestratorAttachmentWorkItemData(
        "test",
        NRSAttachment(
          "test",
          "test",
          "test",
          "test",
          EnvelopeId("test"),
          Some("test")
        ),
        BusinessId("test"),
        "test"
      )
    )

    obj shouldBe Json
      .toJson(obj)(NrsOrchestratorAttachmentWorkItem.formatEncrypted)
      .as[NrsOrchestratorAttachmentWorkItem](NrsOrchestratorAttachmentWorkItem.formatEncrypted)
  }

  "NrsOrchestratorAttachmentWorkItemRepoSpec" should "be able to de-serialise old work-item. All missing data should be replaced with empty " in {
    val oldWorkItem = NrsOrchestratorAttachmentWorkItemOld(
      "test",
      NRSAttachment(
        "test",
        "test",
        "test",
        "test",
        EnvelopeId("test"),
        Some("test")
      ),
      BusinessId("test"),
      "test"
    )
    implicit val writes: Format[NrsOrchestratorAttachmentWorkItemOld] =
      EncryptedFormat.formatEncrypted(jsonCrypto)(Json.format[NrsOrchestratorAttachmentWorkItemOld])
    val readJsonValue = Json
      .toJson(oldWorkItem)
      .as[NrsOrchestratorAttachmentWorkItem](NrsOrchestratorAttachmentWorkItem.formatEncrypted)
    val expected = NrsOrchestratorAttachmentWorkItem(
      EnvelopeId(""),
      FormTemplateId(""),
      SubmissionRef(""),
      NrsOrchestratorAttachmentWorkItemData(
        "test",
        NRSAttachment(
          "test",
          "test",
          "test",
          "test",
          EnvelopeId("test"),
          Some("test")
        ),
        BusinessId("test"),
        "test"
      )
    )
    readJsonValue shouldBe expected
  }
}
