/*
 * Copyright 2017 HM Revenue & Customs
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

package services {
  sealed trait MongoOperation extends Product with Serializable
  case object SaveOperation extends MongoOperation
  case object SaveTolerantOperation extends MongoOperation
  case object UpdateOperation extends MongoOperation
  case object UpdateTolerantOperation extends MongoOperation

  object IsSave {
    def unapply(operation: MongoOperation): Boolean = operation match {
      case SaveOperation | SaveTolerantOperation => true
      case UpdateOperation | UpdateTolerantOperation => false
    }
  }

  object IsUpdate {
    def unapply(operation: MongoOperation): Boolean = !IsSave.unapply(operation)
  }

  object IsTolerant {
    def unapply(operation: MongoOperation): Boolean = operation match {
      case SaveTolerantOperation | UpdateTolerantOperation => true
      case SaveOperation | UpdateOperation => false
    }
  }

  object IsStrict {
    def unapply(operation: MongoOperation): Boolean = !IsTolerant.unapply(operation)
  }
}
