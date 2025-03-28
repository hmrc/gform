/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.it.stubs

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{ ok, stubFor }
import uk.gov.hmrc.gform.repo.DeleteResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils

trait GFormFrontendStubs {

  def gformFrontendFormTemplateCacheStub(formTemplateId: String) =
    stubFor(
      WireMock
        .delete(s"/submissions/formtemplates/$formTemplateId/cache")
        .willReturn(ok(JsonUtils.toJsonStr(DeleteResult(formTemplateId, deleted = true))))
    )

}
