/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.Eq
import cats.syntax.eq._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait FileComponentId {
  def toFileId(): FileId = FileId(value())
  def value(): String = this match {
    case FileComponentId.Single(fcId) => fcId.value
    case FileComponentId.Multi(fcId, index) =>
      "m" + index.toString + "_" + fcId.value
  }

  def isMultiFor(formComponentId: FormComponentId): Boolean = this match {
    case FileComponentId.Single(_)          => false
    case FileComponentId.Multi(fcId, index) => fcId === formComponentId
  }

  def underlyingFormComponentId(): FormComponentId = this match {
    case FileComponentId.Single(fcId)   => fcId
    case FileComponentId.Multi(fcId, _) => fcId
  }

  def maybeIndex: Option[Int] = this match {
    case FileComponentId.Single(_)       => None
    case FileComponentId.Multi(_, index) => Some(index)
  }

  def decrement(): FileComponentId = this match {
    case FileComponentId.Single(fcId)       => FileComponentId.Single(fcId)
    case FileComponentId.Multi(fcId, index) => FileComponentId.Multi(fcId, index - 1)
  }
}

object FileComponentId {

  case class Single(formComponentId: FormComponentId) extends FileComponentId
  case class Multi(formComponentId: FormComponentId, index: Int) extends FileComponentId

  private val MultiFilePrefixRegex = "^m(\\d+)_(.*$)".r

  def fromString(s: String): FileComponentId = s match {
    case MultiFilePrefixRegex(prefix, formComponentIdValue) =>
      Multi(FormComponentId(formComponentIdValue), prefix.toInt)
    case _ => Single(FormComponentId(s))
  }

  implicit val vformat: Format[FileComponentId] =
    implicitly[Format[String]].bimap(fromString(_), _.value())

  implicit val equal: Eq[FileComponentId] = Eq.fromUniversalEquals
}
