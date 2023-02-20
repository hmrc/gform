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
  final case class DateFunctionExpr(path: TemplatePath, dateProjection: DateProjection) extends ReferenceInfo
  final case class PeriodExtExpr(path: TemplatePath, periodExt: PeriodExt) extends ReferenceInfo
  final case class LinkCtxExpr(path: TemplatePath, linkCtx: LinkCtx) extends ReferenceInfo
  final case class DataRetrieveCtxExpr(path: TemplatePath, dataRetrieveCtx: DataRetrieveCtx) extends ReferenceInfo
  final case class DataRetrieveCountExpr(path: TemplatePath, dataRetrieveCount: DataRetrieveCount) extends ReferenceInfo
  final case class SizeExpr(path: TemplatePath, size: Size) extends ReferenceInfo
  final case class CsvCountryCheckExpr(path: TemplatePath, csvCountryCheck: CsvCountryCheck) extends ReferenceInfo
  final case class CsvOverseasCountryCheckExpr(path: TemplatePath, csvOverseasCountryCheck: CsvOverseasCountryCheck)
      extends ReferenceInfo
  final case class CsvCountryCountCheckExpr(path: TemplatePath, csvCountryCountCheck: CsvCountryCountCheck)
      extends ReferenceInfo
  final case class MiniSummaryRowExpr(path: TemplatePath, miniSummaryRow: MiniSummaryRow) extends ReferenceInfo
  final case class IndexOfExpr(path: TemplatePath, indexOf: IndexOf) extends ReferenceInfo
  final case class RemoveSpacesExpr(path: TemplatePath, removeSpaces: RemoveSpaces) extends ReferenceInfo
  final case class NumberedListExpr(path: TemplatePath, numberedList: NumberedList) extends ReferenceInfo
  final case class BulletedListExpr(path: TemplatePath, numberedList: BulletedList) extends ReferenceInfo
}
