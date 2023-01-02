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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.syntax.option._
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName

sealed trait DataSource {
  def convertToString(): String = this match {
    case DataSource.SeissEligible     => DataSource.seiss
    case DataSource.Mongo(collection) => DataSource.mongoPrefix + "." + collection
    case DataSource.Enrolment(serviceName, identifierName) =>
      DataSource.enrolmentPrefix + "." + serviceName.value + "." + identifierName.value
    case DataSource.DelegatedEnrolment(serviceName, identifierName) =>
      DataSource.delegatedEnrolmentPrefix + "." + serviceName.value + "." + identifierName.value
  }
}

object DataSource {
  case object SeissEligible extends DataSource
  case class Mongo(collectionName: CollectionName) extends DataSource
  case class Enrolment(serviceName: ServiceName, identifierName: IdentifierName) extends DataSource
  case class DelegatedEnrolment(serviceName: ServiceName, identifierName: IdentifierName) extends DataSource

  implicit val format: OFormat[DataSource] = derived.oformat()

  def fromString(str: String): Option[DataSource] = str.split("\\.").toList match {
    case `seiss` :: Nil => DataSource.SeissEligible.some
    case `mongoPrefix` :: collectionName :: Nil =>
      DataSource.Mongo(CollectionName(collectionName)).some
    case `enrolmentPrefix` :: serviceName :: identifierName :: Nil =>
      DataSource.Enrolment(ServiceName(serviceName), IdentifierName(identifierName)).some
    case `delegatedEnrolmentPrefix` :: serviceName :: identifierName :: Nil =>
      DataSource.DelegatedEnrolment(ServiceName(serviceName), IdentifierName(identifierName)).some
    case _ => none
  }

  val seiss = "seiss"
  val mongoPrefix = "mongo"
  val enrolmentPrefix = "enrolment"
  val delegatedEnrolmentPrefix = "delegatedEnrolment"

}
