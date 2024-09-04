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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait AddToListRef {
  def allExpressions: NonEmptyList[Expr] = this match {
    case AddToListRef.Basic(formCtx)     => NonEmptyList.one(formCtx)
    case AddToListRef.Expanded(formCtxs) => formCtxs
  }
}

object AddToListRef {
  case class Basic(formCtx: FormCtx) extends AddToListRef
  case class Expanded(formCtx: NonEmptyList[FormCtx]) extends AddToListRef

  import JsonUtils._
  implicit val format: OFormat[AddToListRef] = derived.oformat()
}
