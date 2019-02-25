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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.des

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.des.ContactDetails
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

trait ContactDetailsGen {
  def contactDetailsGen: Gen[ContactDetails] =
    for {
      primaryPhoneNumber   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      secondaryPhoneNumber <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      faxNumber            <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      emailAddress         <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield ContactDetails(primaryPhoneNumber, secondaryPhoneNumber, faxNumber, emailAddress)
}

object ContactDetailsGen extends ContactDetailsGen
