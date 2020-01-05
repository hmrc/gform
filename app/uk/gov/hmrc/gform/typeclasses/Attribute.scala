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

package uk.gov.hmrc.gform.typeclasses

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.xml.Elem

sealed trait Attribute[T] {
  def attribute(name: String, values: List[T]): Elem

  protected def createAttribute(name: String, tpe: String, values: List[String]): Elem = {
    val attributeElem =
      <attribute>
        <attribute_name>{ name }</attribute_name>
        <attribute_type>{ tpe }</attribute_type>
      </attribute>
    val attributeValues = createAttributeValues(values)
    attributeElem.copy(child = attributeElem.child ++ attributeValues)
  }

  private def createAttributeValues(values: List[String]): Elem = {
    val child = values.map(value => <attribute_value>{ value }</attribute_value>)
    val attributeValues = <attribute_values></attribute_values>
    attributeValues.copy(child = child)
  }
}

object Attribute {
  implicit object string extends Attribute[String] {
    def attribute(a: String, c: List[String]): Elem =
      createAttribute(a, "string", c)
  }

  implicit object int extends Attribute[Int] {
    def attribute(a: String, c: List[Int]): Elem =
      createAttribute(a, "int", c.map(_.toString))
  }

  implicit object long extends Attribute[Long] {
    def attribute(a: String, c: List[Long]): Elem =
      createAttribute(a, "integer", c.map(_.toString))
  }

  implicit object localDateTime extends Attribute[LocalDateTime] {
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss")

    def attribute(a: String, c: List[LocalDateTime]): Elem =
      createAttribute(a, "time", c.map(date => date.format(formatter)))
  }
}
