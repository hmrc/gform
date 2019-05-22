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

package uk.gov.hmrc.gform

import java.io.File

import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{ Schema, SchemaFactory, Validator }

import scala.xml.SAXException

object SchemaValidator extends App {

  validate2(
    "/home/pasquale/Downloads/SendApplicationForms.xsd",
    "/home/pasquale/hmrc/dev/gform/ofsted_example_templates/SendApplicationFormSchema.xml")

  def validate(xmlFile: String, xsdFile: String): Boolean = {
    try {
      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val factory = SchemaFactory.newInstance(schemaLang)
      val schema = factory.newSchema(new StreamSource(xsdFile))
      val validator = schema.newValidator()
      validator.validate(new StreamSource(xmlFile))
    } catch {
      case ex: SAXException => ex.printStackTrace(); return false
      case ex: Exception    => ex.printStackTrace()
    }
    true
  }

  def validate2(xmlFile: String, xsdFile: String): Boolean = {
    try {
      val factory: SchemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)

      val schema: Schema = factory.newSchema(new File(xsdFile))
      val validator: Validator = schema.newValidator()

      validator.validate(new StreamSource(new File(xmlFile)))
    } catch {
      case ex: SAXException => ex.printStackTrace(); return false
      case ex: Exception    => ex.printStackTrace()
    }
    true
  }

}
