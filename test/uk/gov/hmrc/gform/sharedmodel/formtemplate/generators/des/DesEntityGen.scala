/*
 * Copyright 2022 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.des.{ DesEntity, Individual, Organisation }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.PrimitiveGen

trait DesEntityGen {
  def desEntityGen: Gen[DesEntity] = Gen.oneOf(organisationGen, individualGen)

  def individualGen: Gen[Individual] =
    for {
      firstName   <- PrimitiveGen.nonEmptyAlphaNumStrGen
      lastName    <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      dateOfBirth <- Gen.option(PrimitiveGen.localDateGen)
    } yield Individual(firstName, lastName, dateOfBirth.map(_.toString))

  def organisationGen: Gen[Organisation] =
    for {
      organisationName <- PrimitiveGen.nonEmptyAlphaNumStrGen
      isAGroup         <- PrimitiveGen.booleanGen
      organisationType <- PrimitiveGen.nonEmptyAlphaNumStrGen
    } yield Organisation(organisationName, isAGroup, organisationType)

}

object DesEntityGen extends DesEntityGen
