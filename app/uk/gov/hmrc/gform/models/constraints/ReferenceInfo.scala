/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.constraints

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

sealed trait ReferenceInfo {
  def path: TemplatePath
}

object ReferenceInfo {
  final case class SumExpr(path: TemplatePath, sum: Sum) extends ReferenceInfo
  final case class CountExpr(path: TemplatePath, count: Count) extends ReferenceInfo
  final case class FormCtxExpr(path: TemplatePath, formCtx: FormCtx) extends ReferenceInfo
  final case class AddressExpr(path: TemplatePath, addressLens: AddressLens) extends ReferenceInfo
  final case class PeriodExpr(path: TemplatePath, period: Period) extends ReferenceInfo
  final case class PeriodExtExpr(path: TemplatePath, periodExt: PeriodExt) extends ReferenceInfo
}
