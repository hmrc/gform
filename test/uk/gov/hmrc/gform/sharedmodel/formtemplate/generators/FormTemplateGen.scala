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
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormTemplateGen {
  def formTemplateIdGen: Gen[FormTemplateId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormTemplateId(_))
  def formNameGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def formTemplateDescriptionGen: Gen[String] = Gen.alphaNumStr
  def developmentPhaseGen: Gen[DevelopmentPhase] = Gen.oneOf(AlphaBanner, BetaBanner, ResearchBanner, LiveBanner)
  def formCategoryGen: Gen[FormCategory] = Gen.oneOf(HMRCReturnForm, HMRCClaimForm, Default)

  def draftRetrievalMethodGen: Gen[DraftRetrievalMethod] =
    for {
      continueOrDeletePage <- ContinueOrDeletePageGen.continueOrDeletePageGen
      draftRetrievalMethod <- Gen.oneOf(
                               OnePerUser(continueOrDeletePage),
                               FormAccessCodeForAgents(continueOrDeletePage),
                               BySubmissionReference)
    } yield draftRetrievalMethod

  def emailTemplateIdGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen

  def emailParameterGen: Gen[EmailParameter] =
    for {
      emailTemplateVariable <- Gen.alphaNumStr
      value                 <- ExprGen.exprGen()

    } yield EmailParameter(emailTemplateVariable, value)

  def emailParameterListGen: Gen[Option[NonEmptyList[EmailParameter]]] =
    Gen.option(PrimitiveGen.oneOrMoreGen(emailParameterGen))

  def templateNameGen: Gen[TemplateName] =
    for {
      templateName <- Gen.alphaNumStr

    } yield TemplateName(templateName)

  def webChatGen: Gen[WebChat] =
    for {
      roomId       <- PrimitiveGen.nonEmptyAlphaNumStrGen
      templateName <- templateNameGen
    } yield WebChat(ChatRoomId(roomId), templateName)

  def toLocalisedString(s: String): LocalisedString = LocalisedString(Map(LangADT.En -> s))

  def formTemplateGen: Gen[FormTemplate] =
    for {
      id                     <- formTemplateIdGen
      name                   <- formNameGen
      description            <- formTemplateDescriptionGen
      developmentPhase       <- Gen.option(developmentPhaseGen)
      category               <- formCategoryGen
      draftRetrievalMethod   <- draftRetrievalMethodGen
      submissionReference    <- Gen.option(FormatExprGen.textExpressionGen)
      destinations           <- DestinationsGen.destinationsGen
      authConfig             <- AuthConfigGen.authConfigGen
      emailTemplateId        <- emailTemplateIdGen
      emailParameters        <- emailParameterListGen
      submitSuccessUrl       <- PrimitiveGen.urlGen
      submitErrorUrl         <- PrimitiveGen.urlGen
      sections               <- PrimitiveGen.zeroOrMoreGen(SectionGen.sectionGen)
      webChat                <- Gen.option(webChatGen)
      acknowledgementSection <- SectionGen.acknowledgementSectionGen
      declarationSection     <- SectionGen.declarationSectionGen
      gFC579Ready            <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield
      FormTemplate(
        id,
        toLocalisedString(name),
        toLocalisedString(description),
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
        webChat,
        sections,
        acknowledgementSection,
        declarationSection,
        gFC579Ready,
        AvailableLanguages.default
      )
}

object FormTemplateGen extends FormTemplateGen
