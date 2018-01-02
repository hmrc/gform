/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import play.api.http.Status
import scala.concurrent.ExecutionContext.Implicits.global

class PingSpec extends support.ITSpec {

  "The application should start and be pingable" in {

    eventually {
      wsclient.GET(s"$baseUrl/ping/ping").futureValue.status shouldBe Status.OK
    }
  }
}
