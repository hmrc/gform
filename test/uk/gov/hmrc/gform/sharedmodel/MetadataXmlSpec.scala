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

package uk.gov.hmrc.gform.sharedmodel

import java.time.LocalDateTime
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.objectstore.{ MetadataXml, ReconciliationId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, DestinationId, DestinationIncludeIf, InstructionPdfFields, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination.Dms
import uk.gov.hmrc.gform.submission._

import scala.xml.{ Elem, Utility }

class MetadataXmlSpec extends Spec {

  private def generateXml(hmrcDms: HmrcDms) = {
    val dmsMetaData = DmsMetaData(formTemplateId = FormTemplateId("some-form-type-id"), "TESTID")

    val submission = Submission(
      submittedDate = LocalDateTime.of(2012, 12, 3, 12, 45),
      submissionRef = SubmissionRef("some-submission-ref"),
      _id = SubmissionId(FormId("some-form-type-id"), EnvelopeId("some-envelope-id")),
      envelopeId = EnvelopeId("some-envelope-id"),
      noOfAttachments = 2,
      dmsMetaData = dmsMetaData
    )

    val pdfSummary = PdfSummary(numberOfPages = 10L, pdfContent = Array.empty[Byte])

    val roboticsAttachmentsName = if (hmrcDms.roboticsAsAttachment.getOrElse(false)) {
      Seq("robotics.xml")
    } else {
      Seq()
    }

    val attachmentNames = Seq("example.pdf", "example2.pdf") ++ roboticsAttachmentsName

    val expectedAttachmentCount = attachmentNames.length

    val attachmentNamesXml: Seq[Elem] = if (hmrcDms.includeAttachmentNames.getOrElse(false)) {
      attachmentNames.map { attachmentName =>
        <attribute>
          <attribute_name>attachment_name</attribute_name>
          <attribute_type>string</attribute_type>
          <attribute_values>
            <attribute_value>{attachmentName}</attribute_value>
          </attribute_values>
        </attribute>
      }
    } else {
      Seq()
    }

    val expected =
      <documents xmlns="http://govtalk.gov.uk/hmrc/gis/content/1">
        <document>
          <header>
            <title>somesubmissionref</title>
            <format>pdf</format>
            <mime_type>application/pdf</mime_type>
            <store>true</store>
            <source>dfs</source>
            <target>DMS</target>
            <reconciliation_id>some-recocilliatin-id</reconciliation_id>
          </header>
          <metadata>
            <attribute>
              <attribute_name>hmrc_time_of_receipt</attribute_name>
              <attribute_type>time</attribute_type>
              <attribute_values>
                <attribute_value>03/12/2012 12:45:00</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>time_xml_created</attribute_name>
              <attribute_type>time</attribute_type>
              <attribute_values>
                <attribute_value>03/12/2012 12:45:00</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>submission_reference</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>somesubmissionref</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>form_id</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-id</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>number_pages</attribute_name>
              <attribute_type>integer</attribute_type>
              <attribute_values>
                <attribute_value>10</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>source</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>dfs</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>customer_id</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>TESTID</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>submission_mark</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>AUDIT_SERVICE</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>cas_key</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>AUDIT_SERVICE</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>classification_type</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-classification-type</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>business_area</attribute_name>
              <attribute_type>string</attribute_type>
              <attribute_values>
                <attribute_value>some-business-area</attribute_value>
              </attribute_values>
            </attribute>
            <attribute>
              <attribute_name>attachment_count</attribute_name>
              <attribute_type>int</attribute_type>
              <attribute_values>
                <attribute_value>{expectedAttachmentCount.toString}</attribute_value>
              </attribute_values>
            </attribute>
            {attachmentNamesXml}
            <attribute>
              <attribute_name>backscan</attribute_name>
              <attribute_type>boolean</attribute_type>
              <attribute_values>
                <attribute_value>true</attribute_value>
              </attribute_values>
            </attribute>
          </metadata>
        </document>
      </documents>

    val metadataXml = MetadataXml
      .getXml(
        submission,
        ReconciliationId("some-recocilliatin-id"),
        pdfSummary.numberOfPages,
        submission.noOfAttachments,
        hmrcDms,
        attachmentNames,
        None
      )

    metadataXml should equal(Utility.trim(expected).asInstanceOf[Elem])(after being streamlined[Elem])
  }

  "metadata.xml" should "be generated" in {

    generateXml(
      HmrcDms(
        DestinationId("TestHmrcDmsId"),
        Dms,
        "some-id",
        Constant("TestHmrcDmsCustomerId"),
        "some-classification-type",
        "some-business-area",
        DestinationIncludeIf.HandlebarValue(""),
        true,
        Some(DataOutputFormat.XML),
        true,
        Some(true),
        Some(InstructionPdfFields.Ordered),
        None,
        None,
        TemplateType.XML,
        None,
        None,
        None,
        None,
        None
      )
    )

  }

  "metadata.xml attachment_count" should "be incremented by 1 if roboticsAsAttachment is true" in {
    generateXml(
      HmrcDms(
        DestinationId("TestHmrcDmsId"),
        Dms,
        "some-id",
        Constant("TestHmrcDmsCustomerId"),
        "some-classification-type",
        "some-business-area",
        DestinationIncludeIf.HandlebarValue(""),
        true,
        Some(DataOutputFormat.XML),
        true,
        Some(true),
        Some(InstructionPdfFields.Ordered),
        None,
        None,
        TemplateType.XML,
        Some(true),
        None,
        None,
        None,
        None
      )
    )
  }

  "metadata.xml attachment_name" should "be added if includeAttachmentNames is true" in {
    generateXml(
      HmrcDms(
        DestinationId("TestHmrcDmsId"),
        Dms,
        "some-id",
        Constant("TestHmrcDmsCustomerId"),
        "some-classification-type",
        "some-business-area",
        DestinationIncludeIf.HandlebarValue(""),
        true,
        Some(DataOutputFormat.XML),
        true,
        Some(true),
        Some(InstructionPdfFields.Ordered),
        None,
        None,
        TemplateType.XML,
        None,
        Some(true),
        None,
        None,
        None
      )
    )
  }

  "metadata.xml attachment_name" should "be added if includeAttachmentNames is true and robotics name should also be added if robotics added as an attachment" in {
    generateXml(
      HmrcDms(
        DestinationId("TestHmrcDmsId"),
        Dms,
        "some-id",
        Constant("TestHmrcDmsCustomerId"),
        "some-classification-type",
        "some-business-area",
        DestinationIncludeIf.HandlebarValue(""),
        true,
        Some(DataOutputFormat.XML),
        true,
        Some(true),
        Some(InstructionPdfFields.Ordered),
        None,
        None,
        TemplateType.XML,
        Some(true),
        Some(true),
        None,
        None,
        None
      )
    )
  }
}
