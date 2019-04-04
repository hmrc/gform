/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.RecalculatedTaxPeriodKey
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ComponentTypeGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen._

trait RecalculatedTaxPeriodKeyGen {
  def recalculatedTaxPeriodKeyGen: Gen[RecalculatedTaxPeriodKey] =
    for {
      id     <- formComponentIdGen
      period <- hmrcTaxPeriodGen
    } yield RecalculatedTaxPeriodKey(id, period)
}

object RecalculatedTaxPeriodKeyGen extends RecalculatedTaxPeriodKeyGen
