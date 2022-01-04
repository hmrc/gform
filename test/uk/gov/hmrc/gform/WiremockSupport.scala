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

package uk.gov.hmrc.gform

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.configureFor
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.options
import org.scalatest.{ BeforeAndAfterAll, Suite }

import scala.util.Random

trait WiremockSupport extends BeforeAndAfterAll { this: Suite =>
  val wiremockPort: Int = 10000 + Random.nextInt(10000)
  val wireMockServer: WireMockServer = new WireMockServer(options().port(wiremockPort))
  val url: String = s"http://localhost:$wiremockPort"

  override def beforeAll(): Unit = startServer()

  override def afterAll(): Unit = stopServer()

  def startServer(): Unit = {
    wireMockServer.start()
    configureFor("localhost", wiremockPort)
  }

  def stopServer(): Unit =
    wireMockServer.stop()
}
