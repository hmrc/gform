/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json._

/** Documents persisted before a defaulted field was added lack that field, and julienrf.json.derived ignores Scala
  * default arguments. Each case here strips a field to emulate an older document.
  */
class PersistedDefaultsSpec extends AnyFlatSpec with Matchers {

  private def stripKey(key: String)(js: JsValue): JsValue = js match {
    case JsObject(fields) => JsObject(fields.collect { case (k, v) if k != key => k -> stripKey(key)(v) }.toMap)
    case JsArray(items)   => JsArray(items.map(stripKey(key)))
    case other            => other
  }

  private def roundTripWithout[A: Format](value: A, missingFields: String*): JsResult[A] = {
    val persisted = Json.toJson(value)
    missingFields.foreach(field => persisted.toString should include(field))
    missingFields.foldLeft(persisted)((acc, field) => stripKey(field)(acc)).validate[A]
  }

  private def decodes[A: Format](value: A, missingFields: String*): org.scalatest.Assertion =
    roundTripWithout(value, missingFields: _*).asOpt shouldBe Some(value)

  "Text persisted without its defaulted fields" should "decode" in {
    val text: ComponentType = Text(ShortText.default, Constant("any text"))
    decodes(text, "removeSpaces", "toUpperCase", "displayWidth")
  }

  "TextArea persisted without its defaulted fields" should "decode" in {
    val textArea: ComponentType = TextArea(ShortText.default, Constant("any text"), dataThreshold = None)
    decodes(textArea, "rows", "displayCharCount", "displayWidth")
  }

  "Number persisted without its defaulted fields" should "decode" in {
    val number: TextConstraint = Number()
    decodes(number, "maxWholeDigits", "maxFractionalDigits", "roundingMode")
  }

  "PositiveNumber persisted without its defaulted fields" should "decode" in {
    val number: TextConstraint = PositiveNumber()
    decodes(number, "maxWholeDigits", "maxFractionalDigits", "roundingMode")
  }
}
