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
      includeIf          <- includeIfGen
      failOnError        <- PrimitiveGen.booleanGen
    } yield
      Destination
        .HmrcDms(id, dmsFormId, customerId, classificationType, businessArea, includeIf, failOnError, false)

  def hmrcDmsGen(includeIf: Option[String] = None, failOnError: Option[Boolean] = None): Gen[Destination.HmrcDms] =
    hmrcDmsGen.map { g =>
      g.copy(includeIf = includeIf.getOrElse(g.includeIf), failOnError = failOnError.getOrElse(g.failOnError))
    }

  def handlebarsHttpApiGen: Gen[Destination.HandlebarsHttpApi] =
    for {
      id          <- destinationIdGen
      profile     <- ProfileNameGen.profileGen
      uri         <- PrimitiveGen.urlContextPathGen
      method      <- HttpMethodGen.httpMethodGen
      payload     <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen).map(_.map(s => s""""$s""""))
      payloadType <- TemplateTypeGen.templateTypeGen
      includeIf   <- includeIfGen
      failOnError <- PrimitiveGen.booleanGen
    } yield Destination.HandlebarsHttpApi(id, profile, uri, method, payload, payloadType, includeIf, failOnError)

  def handlebarsHttpApiGen(
    includeIf: Option[String] = None,
    failOnError: Option[Boolean] = None): Gen[Destination.HandlebarsHttpApi] =
    handlebarsHttpApiGen.map { g =>
      g.copy(includeIf = includeIf.getOrElse(g.includeIf), failOnError = failOnError.getOrElse(g.failOnError))
    }

  def compositeGen: Gen[Destination.Composite] =
    for {
      id           <- destinationIdGen
      includeIf    <- includeIfGen
      destinations <- PrimitiveGen.oneOrMoreGen(singularDestinationGen)
    } yield Destination.Composite(id, includeIf, destinations)

  def stateTransitionGen: Gen[Destination.StateTransition] =
    for {
      id            <- destinationIdGen
      includeIf     <- includeIfGen
      failOnError   <- PrimitiveGen.booleanGen
      requiredState <- FormGen.formStatusGen
    } yield Destination.StateTransition(id, requiredState, includeIf, failOnError)

  def singularDestinationGen: Gen[Destination] =
    Gen.oneOf(hmrcDmsGen, handlebarsHttpApiGen, stateTransitionGen)

  def destinationGen: Gen[Destination] = Gen.frequency(10 -> singularDestinationGen, 1 -> compositeGen)

  def destinationWithFixedIdGen(id: DestinationId): Gen[Destination] = hmrcDmsGen.map(_.copy(id = id))

  def includeIfGen(): Gen[String] = Gen.oneOf(Gen.alphaNumStr, Gen.const("true"), Gen.const("false"))
}

object DestinationGen extends DestinationGen
