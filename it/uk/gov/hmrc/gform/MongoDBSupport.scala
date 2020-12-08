/*
 * Copyright 2020 HM Revenue & Customs
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

import org.apache.commons.lang3.RandomStringUtils
import reactivemongo.bson.BSONObjectID
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{FormTemplate, FormTemplateRaw}
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import uk.gov.hmrc.mongo.{MongoConnector, ReactiveRepository}

trait MongoDBSupport {

  val mongoDbName: String = s"test-${RandomStringUtils.randomNumeric(5)}-${getClass.getSimpleName}"

  val mongoSettings: Map[String, String] = Map(
    "mongodb.uri" -> s"mongodb://localhost:27017/$mongoDbName"
  )

  implicit val mongoConnector = MongoConnector(mongoSettings("mongodb.uri"), failoverStrategy = None)

  val formTemplateRepo = new ReactiveRepository[FormTemplate, BSONObjectID](
    collectionName = "formTemplate",
    mongo = mongoConnector.db,
    domainFormat = FormTemplate.format,
    idFormat = ReactiveMongoFormats.objectIdFormats) {}

  val formTemplateRawRepo = new ReactiveRepository[FormTemplateRaw, BSONObjectID](
    collectionName = "formTemplateRaw",
    mongo = mongoConnector.db,
    domainFormat = FormTemplateRaw.format,
    idFormat = ReactiveMongoFormats.objectIdFormats) {}
}
