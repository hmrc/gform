/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.model

import scala.xml.Elem
import uk.gov.hmrc.bforms.typeclasses.Attribute

object MetadataXml {

  private val xmlDec = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>"""

  private def createMetadata(sap: SubmissionAndPdf): Elem = {
    val submission = sap.submission
    val dmsMetaData = submission.dmsMetaData
    val pdfSummary = sap.pdfSummary
    val attributes = List(
      createAttribute("hmrc_time_of_receipt", submission.submittedDate),
      createAttribute("time_xml_created", submission.submittedDate),
      createAttribute("submission_reference", submission.submissionRef),
      createAttribute("form_id", dmsMetaData.formId),
      createAttribute("number_pages", pdfSummary.numberOfPages),
      createAttribute("source", "dfs"),
      createAttribute("customer_id", "???"),
      createAttribute("submission_mark", submission.submissionMark.getOrElse("???")),
      createAttribute("cas_key", submission.casKey.getOrElse("???")),
      createAttribute("classification_type", dmsMetaData.classificationType),
      createAttribute("business_area", dmsMetaData.businessArea),
      createAttribute("attachment_count", 123)
    )
    <metadata></metadata>.copy(child = attributes)
  }

  private def createHeader(submissionRef: String, reconciliationId: String): Elem = {
    <header>
      <title>{ submissionRef }</title>
      <format>pdf</format>
      <mime_type>application/pdf</mime_type>
      <store>true</store>
      <source>dfs</source>
      <target>DMS</target>
      <reconciliation_id>{ reconciliationId }</reconciliation_id>
    </header>
  }

  private def createDocument(elems: List[Elem]): Elem = {
    <documents xmlns="http://govtalk.gov.uk/hmrc/gis/content/1">
      { <document></document>.copy(child = elems) }
    </documents>
  }

  def getXml(submissionRef: String, reconciliationId: String, sap: SubmissionAndPdf): Elem = {
    val body = List(
      createHeader(submissionRef, reconciliationId),
      createMetadata(sap)
    )

    createDocument(body)
  }

  private def createAttribute[T: Attribute](name: String, value: T): Elem =
    createAttribute(name, List(value))

  private def createAttribute[T: Attribute](name: String, values: List[T]): Elem = {
    implicitly[Attribute[T]].attribute(name, values)
  }
}
