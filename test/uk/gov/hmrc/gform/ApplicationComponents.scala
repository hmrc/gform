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

package uk.gov.hmrc.gform

import org.scalatest.{ BeforeAndAfterAll, TestSuite }
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import play.api.ApplicationLoader.Context
import play.api._

trait ApplicationComponents extends GuiceOneAppPerTest with BeforeAndAfterAll {
  this: TestSuite =>

  override def fakeApplication(): Application = new ApplicationLoader {
    override def load(context: Context): Application = ???
  }.load(context)

  def context: ApplicationLoader.Context = {
    val classLoader = ApplicationLoader.getClass.getClassLoader
    val env = new Environment(new java.io.File("."), classLoader, Mode.Test)
    Context.create(env)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    Play.start(fakeApplication())
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Play.stop(fakeApplication())
  }
}
