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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ JsError, JsSuccess }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Offset

class OffsetSpec extends Spec {

  val signedIntRegex = """(\+|-)?\d+"""
  val convertToInt = (str: String) => if (str.matches(signedIntRegex)) JsSuccess(Offset(str.toInt)) else JsError(s"Couldn't parse Integer from offset, $str")

  "offset as negative integer" should "be parsed successfully" in {
    val result = convertToInt("-5")
    result shouldEqual JsSuccess(Offset(-5))
  }

  "offset as positive" should "be parsed successfully" in {
    val result = convertToInt("15")
    result shouldEqual JsSuccess(Offset(15))
  }

  "offset as float" should "throw exception" in {
    val floatNum: Float = 56f
    val result = convertToInt(floatNum.toString)

    result should be(
      JsError(s"Couldn't parse Integer from offset, $floatNum")
    )
  }

  "offset as double" should "throw exception" in {
    val doubleNum: Double = 122.56
    val result = convertToInt(doubleNum.toString)

    result should be(
      JsError(s"Couldn't parse Integer from offset, $doubleNum")
    )
  }

}
