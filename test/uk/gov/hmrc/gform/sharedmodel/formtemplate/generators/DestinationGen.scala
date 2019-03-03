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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TextExpression
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId }

trait DestinationGen {
  def destinationIdGen: Gen[DestinationId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(DestinationId(_))

  def dmsFormIdGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def classificationTypeGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def customerIdGen: Gen[TextExpression] = FormatExprGen.textExpressionGen
  def businessAreaGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen

  def hmrcDmsGen: Gen[Destination.HmrcDms] =
    for {
      id                 <- destinationIdGen
      dmsFormId          <- dmsFormIdGen
      customerId         <- customerIdGen
      classificationType <- classificationTypeGen
      businessArea       <- businessAreaGen
      includeIf          <- Gen.option(Gen.alphaNumStr)
      failOnError        <- Gen.option(PrimitiveGen.booleanGen)
    } yield Destination.HmrcDms(id, dmsFormId, customerId, classificationType, businessArea, includeIf, failOnError)

  def handlebarsHttpApiGen: Gen[Destination.HandlebarsHttpApi] =
    for {
      id          <- destinationIdGen
      profile     <- ProfileGen.profileGen
      uri         <- PrimitiveGen.urlContextPathGen
      method      <- HttpMethodGen.httpMethodGen
      payload     <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen).map(_.map(s => s""""$s""""))
      includeIf   <- Gen.option(Gen.alphaNumStr)
      failOnError <- Gen.option(PrimitiveGen.booleanGen)
    } yield Destination.HandlebarsHttpApi(id, profile, uri, method, payload, includeIf, failOnError)

  def destinationGen: Gen[Destination] = Gen.oneOf(hmrcDmsGen, handlebarsHttpApiGen)

  def destinationWithFixedIdGen(id: DestinationId): Gen[Destination] = hmrcDmsGen.map(_.copy(id = id))
}

object DestinationGen extends DestinationGen
