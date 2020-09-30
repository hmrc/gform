/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.submission.{ PdfSummary, Submission }
import uk.gov.hmrc.gform.typeclasses.Attribute

import scala.xml.{ Elem, Utility }

object MetadataXml {

  val xmlDec = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>"""

  private def createMetadata(submission: Submission, pdfSummary: PdfSummary, hmrcDms: HmrcDms): Elem = {

    val backscan = hmrcDms.backscan.map(backscan => createAttribute("backscan", backscan)).toList

    val attributes = List(
      createAttribute("hmrc_time_of_receipt", submission.submittedDate),
      createAttribute("time_xml_created", submission.submittedDate),
      createAttribute("submission_reference", submission.submissionRef.withoutHyphens),
      createAttribute("form_id", hmrcDms.dmsFormId),
      createAttribute("number_pages", pdfSummary.numberOfPages),
      createAttribute("source", "dfs"),
      createAttribute("customer_id", submission.dmsMetaData.customerId),
      createAttribute("submission_mark", "AUDIT_SERVICE"), // We are not using CAS
      createAttribute("cas_key", "AUDIT_SERVICE"), // We are not using CAS
      createAttribute("classification_type", hmrcDms.classificationType),
      createAttribute("business_area", hmrcDms.businessArea),
      createAttribute("attachment_count", submission.noOfAttachments)
    ) ++ backscan

    <metadata></metadata>.copy(child = attributes)
  }

  private def createHeader(submissionRef: SubmissionRef, reconciliationId: ReconciliationId): Elem =
    <header>
      <title>{ submissionRef.withoutHyphens }</title>
      <format>pdf</format>
      <mime_type>application/pdf</mime_type>
      <store>true</store>
      <source>dfs</source>
      <target>DMS</target>
      <reconciliation_id>{ reconciliationId.value }</reconciliation_id>
    </header>

  private def createDocument(elems: List[Elem]): Elem =
    <documents xmlns="http://govtalk.gov.uk/hmrc/gis/content/1">
      { <document></document>.copy(child = elems) }
    </documents>

  def getXml(
    submission: Submission,
    reconciliationId: ReconciliationId,
    pdfSummary: PdfSummary,
    hmrcDms: HmrcDms): Elem = {
    val body =
      List(createHeader(submission.submissionRef, reconciliationId), createMetadata(submission, pdfSummary, hmrcDms))

    trim(createDocument(body))
  }

  private def trim(e: Elem): Elem = Utility.trim(e).asInstanceOf[Elem]

  private def createAttribute[T: Attribute](name: String, value: T): Elem =
    createAttribute(name, List(value))

  private def createAttribute[T: Attribute](name: String, values: List[T]): Elem =
    implicitly[Attribute[T]].attribute(name, values)
}
