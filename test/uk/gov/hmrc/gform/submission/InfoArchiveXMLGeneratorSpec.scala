/*
 * Copyright 2025 HM Revenue & Customs
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

import org.scalacheck.Gen
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.DestinationResult
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen
import uk.gov.hmrc.gform.submission.destinations.SubmissionGen.submissionGen

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneId }
import scala.xml.PrettyPrinter

class InfoArchiveXMLGeneratorSpec extends Spec {

  private val prettyPrinter = new PrettyPrinter(1000, 2, minimizeEmpty = true)

  private val destinationResultGen: Gen[DestinationResult] =
    for {
      destinationId        <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(DestinationId(_))
      includeIf            <- Gen.option(PrimitiveGen.booleanGen)
      customerId           <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      taxpayerId           <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      paymentReference     <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      nino                 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      utr                  <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      postalCode           <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      pegaCaseId           <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      bankAccountName      <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      sortCode             <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      accountNumber        <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      rollNumber           <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      refundClaimReference <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield DestinationResult(
      destinationId,
      includeIf,
      customerId,
      taxpayerId,
      paymentReference,
      nino,
      utr,
      postalCode,
      pegaCaseId,
      bankAccountName,
      sortCode,
      accountNumber,
      rollNumber,
      refundClaimReference
    )

  "generatePdi" should "generate the correct XML for submission information package" in {
    val attachmentName = "attachment"
    val hash = "hash"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")

    for {
      submission        <- submissionGen
      destinationResult <- Gen.option(destinationResultGen)
    } yield {
      val expectedPdi =
        <vatc2carchives xmlns="urn:hmrc:en:xsd:vatc2carchive.gform.1.0">
          <gform>
            <CorrelationID>
              {submission.envelopeId.value}
            </CorrelationID>
            <FormType>Charge</FormType>
            <SubmssionDate>
              {submission.submittedDate.format(formatter)}
            </SubmssionDate>
            <NINO>
              {destinationResult.flatMap(_.nino).getOrElse("")}
            </NINO>
            <UTR>
              {destinationResult.flatMap(_.utr).getOrElse("")}
            </UTR>
            <PostCode>
              {destinationResult.flatMap(_.postalCode).getOrElse("")}
            </PostCode>
            <SubmissionReference>
              {submission.submissionRef}
            </SubmissionReference>
            <PaymentReference>
              {destinationResult.flatMap(_.paymentReference).getOrElse("")}
            </PaymentReference>
            <Attachments>
              <Attachment>
                <AttachmentName>
                  {attachmentName}
                </AttachmentName>
                <FileName>
                  {attachmentName}
                  .pdf</FileName>
                <MimeType>application/pdf</MimeType>
                <Hash>
                  {hash}
                </Hash>
                <CreatedBy>GForm</CreatedBy>
                <CreatedOnDate>
                  {formatter.format(Instant.now.atZone(ZoneId.of("Europe/London")))}
                </CreatedOnDate>
              </Attachment>
            </Attachments>
          </gform>
        </vatc2carchives>

      val result = InfoArchiveXMLGenerator.generatePdi(submission, destinationResult, attachmentName, hash)
      prettyPrinter.formatNodes(result) shouldBe prettyPrinter.formatNodes(expectedPdi)
    }
  }

  "generateSip" should "generate the correct XML for preservation description information" in {
    val envelopeId = EnvelopeId("envelope")
    val date = "date"

    val expectedSip =
      <sip xmlns="urn:x-emc:ia:schema:sip:1.0">
        <dss>
          <holding>VATC2CHolding</holding>
          <id>GFDA_{envelopeId.value}</id>
          <pdi_schema>urn:hmrc:en:xsd:vatc2carchive.gform.1.0</pdi_schema>
          <production_date>{date}</production_date>
          <base_retention_date>{date}</base_retention_date>
          <producer>GForm</producer>
          <entity>OperationalReadiness</entity>
          <priority>0</priority>
          <application>VATC2C</application>
          <retention_class>vatc2cdataretention</retention_class>
        </dss>
        <production_date>{date}</production_date>
        <seqno>1</seqno>
        <is_last>true</is_last>
        <aiu_count>1</aiu_count>
        <page_count>0</page_count>
      </sip>

    val result = InfoArchiveXMLGenerator.generateSip(envelopeId, date)
    prettyPrinter.formatNodes(result) shouldBe prettyPrinter.formatNodes(expectedSip)
  }

}
