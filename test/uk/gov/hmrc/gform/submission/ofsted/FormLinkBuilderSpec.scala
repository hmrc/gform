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
import uk.gov.hmrc.gform.sharedmodel.form.FormId

class FormLinkBuilderSpec extends Spec {

  it should "build a link to the form to be reviewed" in new FormLinkBuilder {
    buildLink(FormId("123")) shouldBe FormLink("http://localhost:9195/submissions/form/123")
  }
}
