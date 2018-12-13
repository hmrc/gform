/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Destination, DestinationId }

object DestinationGen {
  def destinationIdGen: Gen[DestinationId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(DestinationId(_))

  def hmrcDmsGen: Gen[Destination.HmrcDms] =
    for {
      id                 <- destinationIdGen
      dmsFormId          <- PrimitiveGen.nonEmptyAlphaNumStrGen
      customerId         <- FormatExprGen.textExpressionGen
      classificationType <- PrimitiveGen.nonEmptyAlphaNumStrGen
      businessArea       <- PrimitiveGen.nonEmptyAlphaNumStrGen
    } yield Destination.HmrcDms(id, dmsFormId, customerId, classificationType, businessArea)

  def destinationGen: Gen[Destination] = hmrcDmsGen

  def destinationWithFixedIdGen(id: DestinationId): Gen[Destination] = hmrcDmsGen.map(_.copy(id = id))
}
