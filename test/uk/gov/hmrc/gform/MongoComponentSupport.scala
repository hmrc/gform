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

import uk.gov.hmrc.mongo.MongoComponent

import scala.util.Random

trait MongoComponentSupport {
  val mongoDbName: String = s"test-${randomNumeric(5)}-${getClass.getSimpleName}"
  val mongoDBURI: String = s"mongodb://localhost:27017/$mongoDbName"

  val mongoComponent: MongoComponent = MongoComponent(mongoDBURI)

  def randomNumeric(length: Int): String =
    (1 to length).map(_ => Random.nextInt(10)).mkString
}
