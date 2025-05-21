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

import uk.gov.hmrc.gform.sharedmodel.DestinationResult
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId

import java.time.format.DateTimeFormatter
import java.time.{ Instant, ZoneId }
import scala.xml.NodeSeq

object InfoArchiveXMLGenerator {
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")

  def generatePdi(
    submission: Submission,
    destinationResult: Option[DestinationResult],
    attachmentName: String,
    hash: String
  ): NodeSeq =
    <vatc2carchives xmlns="urn:hmrc:en:xsd:vatc2carchive.gform.1.0">
    <gform>
          <CorrelationID>{submission.envelopeId.value}</CorrelationID >
          <FormType>Charge</FormType>
          <SubmissionDate>{submission.submittedDate.format(formatter)}</SubmissionDate>
          {destinationResult.flatMap(r => r.nino.filter(_.nonEmpty).map(nino => <NINO>{nino}</NINO>)).getOrElse(NodeSeq.Empty)}
          {destinationResult.flatMap(r => r.utr.filter(_.nonEmpty).map(utr => <UTR>{utr}</UTR>)).getOrElse(NodeSeq.Empty)}
          {destinationResult.flatMap(r => r.postalCode.filter(_.nonEmpty).map(postalCode => <PostCode>{postalCode}</PostCode>)).getOrElse(NodeSeq.Empty)}
          <SubmissionReference>{submission.submissionRef}</SubmissionReference>
          <PaymentReference>{destinationResult.flatMap(_.paymentReference).getOrElse("")}</PaymentReference>
          <Attachments>
            <Attachment>
              <AttachmentName>{attachmentName}</AttachmentName>
              <FileName>{attachmentName}.pdf</FileName>
              <MimeType>application/pdf</MimeType>
              <Hash>{hash}</Hash>
              <CreatedBy>GForm</CreatedBy>
              <CreatedOnDate>{formatter.format(Instant.now.atZone(ZoneId.of("Europe/London")))}</CreatedOnDate>
            </Attachment>
          </Attachments>
    </gform>
  </vatc2carchives>

  def generateSip(envelopeId: EnvelopeId, date: String): NodeSeq =
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
}
