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

package uk.gov.hmrc.gform.typeclasses

import java.time.LocalDateTime

import uk.gov.hmrc.gform.Spec

import scala.xml.Elem

class AttributeSpec extends Spec {

  "Attribute typesclass" should "return xml with 'integer' type for Long type" in {

    val xml = implicitly[Attribute[Long]].attribute("some-name", List(10))

    val expected = <attribute>
                     <attribute_name>some-name</attribute_name>
                     <attribute_type>integer</attribute_type>
                     <attribute_values><attribute_value>10</attribute_value></attribute_values>
                   </attribute>

    xml should equal(expected)(after being streamlined[Elem])
  }

  it should "return xml with 'int' type for Int type" in {

    val xml = implicitly[Attribute[Int]].attribute("some-name", List(10))

    val expected = <attribute>
                     <attribute_name>some-name</attribute_name>
                     <attribute_type>int</attribute_type>
                     <attribute_values><attribute_value>10</attribute_value></attribute_values>
                   </attribute>

    xml should equal(expected)(after being streamlined[Elem])
  }

  it should "return xml with 'string' type for String type" in {

    val xml = implicitly[Attribute[String]].attribute("some-name", List("abc"))

    val expected = <attribute>
                     <attribute_name>some-name</attribute_name>
                     <attribute_type>string</attribute_type>
                     <attribute_values><attribute_value>abc</attribute_value></attribute_values>
                   </attribute>

    xml should equal(expected)(after being streamlined[Elem])
  }

  it should "return xml with 'time' type for LocalDateTime type" in {

    val xml =
      implicitly[Attribute[LocalDateTime]].attribute("some-name", List(LocalDateTime.of(2012, 12, 3, 12, 34, 56)))

    val expected = <attribute>
                     <attribute_name>some-name</attribute_name>
                     <attribute_type>time</attribute_type>
                     <attribute_values><attribute_value>03/12/2012 12:34:56</attribute_value></attribute_values>
                   </attribute>

    xml should equal(expected)(after being streamlined[Elem])
  }
}
