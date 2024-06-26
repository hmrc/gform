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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.FunSuite
import play.api.libs.json.{ JsString, JsSuccess }

class ComponentTypeRawSpec extends FunSuite {
  test("deserialize summaryList") {
    JsString("miniSummaryList").validate[ComponentTypeRaw] shouldBe JsSuccess(SummaryListRaw)
  }

  test("deserialize table") {
    JsString("table").validate[ComponentTypeRaw] shouldBe JsSuccess(TableCompRaw)
  }
}
