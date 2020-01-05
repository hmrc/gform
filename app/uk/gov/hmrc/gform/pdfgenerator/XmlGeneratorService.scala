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

package uk.gov.hmrc.gform.pdfgenerator

import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Attribute

import scala.collection.immutable.List
import scala.xml.{ Elem, Utility }

object XmlGeneratorService extends XmlGeneratorService

trait XmlGeneratorService {

  val xmlDec = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>"""

  private def createAttribute[T: Attribute](name: String, value: T): Elem =
    createAttribute(name, List(value))

  private def createAttribute[T: Attribute](name: String, values: List[T]): Elem =
    implicitly[Attribute[T]].attribute(name, values)

  private def createFieldData(formFields: List[FormField], formComponent: FormComponent): List[Elem] =
    if (formComponent.submissible)
      formFields.map(formField => createAttribute(formField.id.toString, formField.value))
    else
      List()

  private def createSectionData(section: SectionFormField): List[Elem] =
    section.fields.flatMap(field => createFieldData(field._1, field._2))

  private def createSubmissionData(sectionFormFields: List[SectionFormField]): Elem = {
    val attributes = sectionFormFields.flatMap(section => createSectionData(section))
    <submission></submission>.copy(child = attributes)
  }

  private def createHeader(submissionRef: SubmissionRef): Elem =
    <header>
      <title>{ submissionRef.value.replace("-", "") }</title>
      <source>gform</source>
      <target>DMS</target>
    </header>

  // TODO header etc.
  private def createDocument(elems: List[Elem]): Elem =
    <documents>
      { <document></document>.copy(child = elems) }
    </documents>

  private def trim(e: Elem): Elem = Utility.trim(e).asInstanceOf[Elem]

  def getXml(sectionFormFields: List[SectionFormField], submissionRef: SubmissionRef): Elem = {
    val body = List(createHeader(submissionRef), createSubmissionData(sectionFormFields))

    trim(createDocument(body))
  }

}
