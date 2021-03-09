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
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

trait DesRegistrationResponseGen {
  def desRegistrationResponseGen: Gen[DesRegistrationResponse] =
    for {
      safeId               <- PrimitiveGen.nonEmptyAlphaNumStrGen
      agentReferenceNumber <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      sapNumber            <- PrimitiveGen.nonEmptyAlphaNumStrGen
      isEditable           <- PrimitiveGen.booleanGen
      isAnAgent            <- PrimitiveGen.booleanGen
      isAnASAgent          <- PrimitiveGen.booleanGen
      isAnIndividual       <- PrimitiveGen.booleanGen
      orgOrInd             <- DesEntityGen.desEntityGen
      address              <- AddressGen.addressGen
      contactDetails       <- Gen.option(ContactDetailsGen.contactDetailsGen)
    } yield DesRegistrationResponse(
      safeId,
      agentReferenceNumber,
      sapNumber,
      isEditable,
      isAnAgent,
      isAnASAgent,
      isAnIndividual,
      orgOrInd,
      address,
      contactDetails
    )
}

object DesRegistrationResponseGen extends DesRegistrationResponseGen
