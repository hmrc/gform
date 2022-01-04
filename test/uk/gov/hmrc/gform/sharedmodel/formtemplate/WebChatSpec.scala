/*
 * Copyright 2022 HM Revenue & Customs
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

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.Json
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormTemplateGen

class WebChatSpec extends Spec with ScalaCheckDrivenPropertyChecks {

  "WebChat" should "round trip derived json" in {
    forAll(FormTemplateGen.webChatGen) { obj =>
      WebChat.format.reads(WebChat.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  "WebChat" should "read the following json correctly" in {

    WebChat.format
      .reads(
        Json.parse(
          s"""|{
              |  "chatRoomId": "1001",
              |  "templateName": "hmrc6"
              |}""".stripMargin
        )
      )
      .get shouldBe WebChat(ChatRoomId("1001"), TemplateName("hmrc6"))
  }

  "WebChat" should "read the following json with no templateName and get the default" in {

    WebChat.format
      .reads(
        Json.parse(
          s"""|{
              |  "chatRoomId": "1001"
              |}""".stripMargin
        )
      )
      .get shouldBe WebChat(ChatRoomId("1001"), TemplateName("hmrc7"))
  }

}
