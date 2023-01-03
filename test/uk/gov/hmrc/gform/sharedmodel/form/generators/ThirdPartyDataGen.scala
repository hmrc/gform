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

package uk.gov.hmrc.gform.sharedmodel.form.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormComponentGen, PrimitiveGen }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.des.DesRegistrationResponseGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.ObligationsGen

trait ThirdPartyDataGen {
  def thirdPartyDataGen =
    for {
      desRegistrationResponse <- Gen.option(DesRegistrationResponseGen.desRegistrationResponseGen)
      obligations             <- ObligationsGen.obligationsGen
      emailVerification <- PrimitiveGen.possiblyEmptyMapGen(
                             FormComponentGen.formComponentIdGen,
                             EmailVerificationCodeGen.emailVerificationCodeGen
                           )
      queryParams <- QueryParamsGen.queryParamsGen
    } yield ThirdPartyData(
      desRegistrationResponse,
      obligations,
      emailVerification,
      queryParams,
      None,
      BooleanExprCache.empty,
      None,
      None,
      None,
      None,
      None,
      None
    )
}

object ThirdPartyDataGen extends ThirdPartyDataGen
