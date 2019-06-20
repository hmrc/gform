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

package uk.gov.hmrc.gform.cygnum

import cats.Id
import uk.gov.hmrc.gform.Spec

class CygnumDataExtractorTest extends Spec {

  it should "extract URN from cygnum response" in new CygnumDataExtractor[Id](new CygnumDataExtractorProgram[Id]) {
    extractUrn(response) shouldBe "2502244"
  }

  private val response =
    """<s:Envelope>
      |    <s:Header>
      |    </s:Header>
      |    <s:Body>
      |        <GetDataResponse>
      |            <GetDataResult>&lt;?xml version="1.0" encoding="utf-8"?&gt;&lt;URNs
      |                xmlns:xs="http://www.w3.org/2001/XMLSchema-instance"&gt;&lt;URN&gt;2502244&lt;/URN&gt;&lt;/URNs&gt;
      |            </GetDataResult>
      |        </GetDataResponse>
      |    </s:Body>
      |</s:Envelope>
    """.stripMargin
}
