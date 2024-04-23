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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen.formComponentIdGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SectionGen.pdfContextGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ LayoutDisplayWidth, SummarySection }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SmartStringGen.smartStringGen

trait SummarySectionGen {
  def summarySectionGen: Gen[SummarySection] =
    for {
      title          <- smartStringGen
      caption        <- Gen.option(smartStringGen)
      header         <- smartStringGen
      footer         <- smartStringGen
      continueLabel  <- Gen.option(smartStringGen)
      fields         <- Gen.option(PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen()))
      pdf            <- Gen.option(pdfContextGen)
      excludeFromPdf <- Gen.option(PrimitiveGen.zeroOrMoreGen(formComponentIdGen))
    } yield SummarySection(
      title,
      caption,
      header,
      footer,
      continueLabel,
      fields,
      LayoutDisplayWidth.M,
      None,
      pdf,
      excludeFromPdf
    )
}

object SummarySectionGen extends SummarySectionGen
