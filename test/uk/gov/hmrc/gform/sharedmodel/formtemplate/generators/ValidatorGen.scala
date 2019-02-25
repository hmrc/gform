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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BankAccoutnModulusCheck, HMRCUTRPostcodeCheckValidator, Validator }

trait ValidatorGen {
  def hMRCUTRPostcodeCheckValidatorGen: Gen[HMRCUTRPostcodeCheckValidator] =
    for {
      errorMessage <- Gen.alphaNumStr
      regime       <- Gen.alphaNumStr
      utr          <- ExprGen.formCtxGen
      postcode     <- ExprGen.formCtxGen
    } yield HMRCUTRPostcodeCheckValidator(errorMessage, regime, utr, postcode)

  def bankAccountModulusCheckGen: Gen[BankAccoutnModulusCheck] =
    for {
      errorMessage  <- Gen.alphaNumStr
      sortCode      <- ExprGen.formCtxGen
      accountNumber <- ExprGen.formCtxGen
    } yield BankAccoutnModulusCheck(errorMessage, accountNumber, sortCode)

  def validatorGen: Gen[Validator] = Gen.oneOf(hMRCUTRPostcodeCheckValidatorGen, bankAccountModulusCheckGen)
}

object ValidatorGen extends ValidatorGen
