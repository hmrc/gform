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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.des

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.des.{ Address, InternationalAddress, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

trait AddressGen {
  def addressGen: Gen[Address] = Gen.oneOf(ukAddressGen, internationalAddressGen)

  def ukAddressGen: Gen[UkAddress] =
    for {
      addressLine1 <- PrimitiveGen.nonEmptyAlphaNumStrGen
      addressLine2 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      addressLine3 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      addressLine4 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      postalCode   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield UkAddress(addressLine1, addressLine2, addressLine3, addressLine4, postalCode)

  def internationalAddressGen: Gen[InternationalAddress] =
    for {
      addressLine1 <- PrimitiveGen.nonEmptyAlphaNumStrGen
      addressLine2 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      addressLine3 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      addressLine4 <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      countryCode  <- PrimitiveGen.desInternationalCountryCodeGen
      postalCode   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield InternationalAddress(addressLine1, addressLine2, addressLine3, addressLine4, countryCode, postalCode)

}

object AddressGen extends AddressGen
