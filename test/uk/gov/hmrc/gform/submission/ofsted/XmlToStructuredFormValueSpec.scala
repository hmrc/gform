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

package uk.gov.hmrc.gform.submission.ofsted
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue._

class XmlToStructuredFormValueSpec extends Spec {
  "apply" should "produce an empty ObjectStructure given an element with no children and no text" in {
    XmlToStructuredFormValue(<foo></foo>) shouldBe ObjectStructure(Nil)
  }

  it should "produce a TextNode if the element contains only text" in {
    XmlToStructuredFormValue(<foo>hello</foo>) shouldBe TextNode("hello")
  }

  it should "produce an Object structure containing arrays of child elements if there are only child elements" in {
    verifyObjectStructure(
      XmlToStructuredFormValue(<foo><bar>Hello</bar><bar>World</bar><baz>Goodbye</baz></foo>),
      ObjectStructure(
        List(
          Field(FieldName("bar"), ArrayNode(List(TextNode("Hello"), TextNode("World")))),
          Field(FieldName("baz"), ArrayNode(List(TextNode("Goodbye"))))
        ))
    )
  }

  it should "produce an Object structure containing arrays of child elements and ignore any text if there are mixtures of text and child elements" in {
    verifyObjectStructure(
      XmlToStructuredFormValue(<foo><bar>Hello</bar>ignored<bar>World</bar><baz>Goodbye</baz></foo>),
      ObjectStructure(
        List(
          Field(FieldName("bar"), ArrayNode(List(TextNode("Hello"), TextNode("World")))),
          Field(FieldName("baz"), ArrayNode(List(TextNode("Goodbye"))))
        ))
    )
  }

  private def verifyObjectStructure(actual: StructuredFormValue, expected: ObjectStructure) = actual match {
    case ObjectStructure(fields) =>
      fields.sortBy(_.name.name) shouldBe expected.fields.sortBy(_.name.name)
    case v => fail(v.toString)
  }
}
