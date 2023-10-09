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

package uk.gov.hmrc.gform.builder

import io.circe.parser._
import io.circe.syntax._
import io.circe._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UpdateRequestDecoderSpec extends AnyFlatSpec with Matchers {

  import Decoders._
  "SectionDetails Decoder" should "decode correctly" in {
    val json = """
      {
        "sectionData": {"field": "value"},
        "sectionPath": "test/path"
      }
    """
    val expected = SectionDetails(Json.obj("field" -> "value".asJson), "test/path")

    decode[SectionDetails](json) shouldEqual Right(expected)
  }

  "ComponentUpdateRequest Decoder" should "decode correctly" in {
    val json = """
      {
        "field": "value",
        "sectionDetails": {
          "sectionData": {"field": "value"},
          "sectionPath": "test/path"
        }
      }
    """
    val sectionDetails = SectionDetails(Json.obj("field" -> "value".asJson), "test/path")
    val expected = ComponentUpdateRequest(Json.obj("field" -> "value".asJson), Some(sectionDetails))

    decode[ComponentUpdateRequest](json) shouldEqual Right(expected)
  }

  "ComponentDetails Decoder" should "decode correctly" in {
    val json = """
      {
        "componentData": {"field": "value"}
      }
    """
    val expected = ComponentDetails(Json.obj("field" -> "value".asJson))

    decode[ComponentDetails](json) shouldEqual Right(expected)
  }

  "SectionUpdateRequest Decoder" should "decode correctly" in {
    val json = """
      {
        "field": "value",
        "componentDetails": {
          "componentData": {"field": "value"}
        }
      }
    """
    val componentDetails = ComponentDetails(Json.obj("field" -> "value".asJson))
    val expected = SectionUpdateRequest(Json.obj("field" -> "value".asJson), Some(componentDetails))

    decode[SectionUpdateRequest](json) shouldEqual Right(expected)
  }
}
