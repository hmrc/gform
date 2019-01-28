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
import cats.data.NonEmptyList
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormTemplateGen {
  def formTemplateIdGen: Gen[FormTemplateId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormTemplateId(_))
  def formNameGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def formTemplateDescriptionGen: Gen[String] = Gen.alphaNumStr
  def developmentPhaseGen: Gen[DevelopmentPhase] = Gen.oneOf(AlphaBanner, BetaBanner, ResearchBanner, LiveBanner)
  def formCategoryGen: Gen[FormCategory] = Gen.oneOf(HMRCReturnForm, HMRCClaimForm, Default)
  def draftRetrievalMethodGen: Gen[DraftRetrievalMethod] = Gen.oneOf(OnePerUser, FormAccessCodeForAgents)
  def emailTemplateIdGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen

  def emailParameterGen: Gen[EmailParameter] =
    for {
      emailTemplateVariable <- Gen.alphaNumStr
      value                 <- Gen.alphaNumStr

    } yield EmailParameter(emailTemplateVariable, value)

  def emailParameterListGen: Gen[Option[NonEmptyList[EmailParameter]]] =
    Gen.option(PrimitiveGen.oneOrMoreGen(emailParameterGen))

  def formTemplateGen: Gen[FormTemplate] =
    for {
      id                     <- formTemplateIdGen
      name                   <- formNameGen
      description            <- formTemplateDescriptionGen
      developmentPhase       <- Gen.option(developmentPhaseGen)
      category               <- Gen.option(formCategoryGen)
      draftRetrievalMethod   <- Gen.option(draftRetrievalMethodGen)
      submissionReference    <- Gen.option(FormatExprGen.textExpressionGen)
      destinations           <- DestinationsGen.destinationsGen
      authConfig             <- AuthConfigGen.authConfigGen
      emailTemplateId        <- emailTemplateIdGen
      emailParameters        <- emailParameterListGen
      submitSuccessUrl       <- PrimitiveGen.urlGen
      submitErrorUrl         <- PrimitiveGen.urlGen
      sections               <- PrimitiveGen.zeroOrMoreGen(SectionGen.sectionGen)
      acknowledgementSection <- SectionGen.acknowledgementSectionGen
      declarationSection     <- SectionGen.declarationSectionGen
      gFC579Ready            <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield
      FormTemplate(
        id,
        name,
        description,
        developmentPhase,
        category,
        draftRetrievalMethod,
        submissionReference,
        destinations,
        authConfig,
        emailTemplateId,
        emailParameters,
        submitSuccessUrl,
        submitErrorUrl,
        sections,
        acknowledgementSection,
        declarationSection,
        gFC579Ready
      )
}

object FormTemplateGen extends FormTemplateGen
