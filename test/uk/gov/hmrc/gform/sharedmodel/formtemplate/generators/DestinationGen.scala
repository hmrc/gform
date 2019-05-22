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

  def handlebarsHttpApiGen: Gen[Destination.HandlebarsHttpApi] =
    for {
      id          <- destinationIdGen
      profile     <- ProfileNameGen.profileGen
      uri         <- PrimitiveGen.urlContextPathGen
      method      <- HttpMethodGen.httpMethodGen
      payload     <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen).map(_.map(s => s""""$s""""))
      includeIf   <- includeIfGen
      failOnError <- PrimitiveGen.booleanGen
    } yield Destination.HandlebarsHttpApi(id, profile, uri, method, payload, includeIf, failOnError)

  def reviewingOfstedGen: Gen[Destination.ReviewingOfsted] =
    for {
      id          <- destinationIdGen
      cfid        <- FormComponentGen.formComponentIdGen
      rtid        <- FormTemplateGen.formTemplateIdGen
      userId      <- UserIdGen.userIdGen
      includeIf   <- includeIfGen
      failOnError <- PrimitiveGen.booleanGen
    } yield Destination.ReviewingOfsted(id, cfid, rtid, userId, includeIf, failOnError)

  def reviewRejectionGen: Gen[Destination.ReviewRejection] =
    for {
      id                       <- destinationIdGen
      correlationFieldId       <- FormComponentGen.formComponentIdGen
      reviewFormCommentFieldId <- FormComponentGen.formComponentIdGen
      includeIf                <- includeIfGen
      failOnError              <- PrimitiveGen.booleanGen
    } yield Destination.ReviewRejection(id, correlationFieldId, reviewFormCommentFieldId, includeIf, failOnError)

  def reviewApprovalGen: Gen[Destination.ReviewApproval] =
    for {
      id                 <- destinationIdGen
      correlationFieldId <- FormComponentGen.formComponentIdGen
      includeIf          <- includeIfGen
      failOnError        <- PrimitiveGen.booleanGen
    } yield Destination.ReviewApproval(id, correlationFieldId, includeIf, failOnError)

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
    Gen.oneOf(
      hmrcDmsGen,
      handlebarsHttpApiGen,
      reviewingOfstedGen,
      reviewRejectionGen,
      reviewApprovalGen,
      stateTransitionGen)

  def destinationGen: Gen[Destination] = Gen.frequency(10 -> singularDestinationGen, 1 -> compositeGen)

  def destinationWithFixedIdGen(id: DestinationId): Gen[Destination] = hmrcDmsGen.map(_.copy(id = id))

  def includeIfGen(): Gen[String] = Gen.oneOf(Gen.alphaNumStr, Gen.const("true"), Gen.const("false"))
}

object DestinationGen extends DestinationGen
