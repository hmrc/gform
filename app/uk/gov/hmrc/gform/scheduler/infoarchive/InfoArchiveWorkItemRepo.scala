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

package uk.gov.hmrc.gform.scheduler.infoarchive

import org.mongodb.scala.model.{ IndexModel, IndexOptions, Indexes }
import uk.gov.hmrc.gform.scheduler.WorkItemRepo
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesWorkItem
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext

class InfoArchiveWorkItemRepo(mongoComponent: MongoComponent)(implicit ec: ExecutionContext)
    extends WorkItemRepo[SdesWorkItem](
      mongoComponent,
      "infoArchiveWorkItem",
      extraIndexes = Seq(
        IndexModel(
          Indexes.ascending("item.formTemplateId"),
          IndexOptions()
            .background(true)
            .name("item.formTemplateId")
        )
      )
    )
