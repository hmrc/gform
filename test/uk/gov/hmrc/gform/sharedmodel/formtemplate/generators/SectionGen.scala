/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SmartStringGen.smartStringGen

trait SectionGen {

  def verifierRecipeGen: Gen[VerifierRecipe] =
    for {
      key     <- PrimitiveGen.nonEmptyAlphaNumStrGen
      formCtx <- ExprGen.formCtxGen
    } yield VerifierRecipe(key, formCtx)

  def identifierRecipeGen: Gen[IdentifierRecipe] =
    for {
      key     <- PrimitiveGen.nonEmptyAlphaNumStrGen
      formCtx <- ExprGen.formCtxGen
    } yield IdentifierRecipe(key, formCtx)

  def enrolmentSectionGen: Gen[EnrolmentSection] =
    for {
      title         <- smartStringGen
      noPIITitle    <- Gen.option(smartStringGen)
      shortName     <- Gen.option(smartStringGen)
      fields        <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      identifiers   <- PrimitiveGen.oneOrMoreGen(identifierRecipeGen)
      verifiers     <- PrimitiveGen.zeroOrMoreGen(verifierRecipeGen)
      continueLabel <- Gen.option(smartStringGen)
    } yield EnrolmentSection(title, noPIITitle, shortName, fields.toList, identifiers, verifiers, continueLabel)

  def acknowledgementSectionGen: Gen[AcknowledgementSection] =
    for {
      title               <- Gen.option(smartStringGen)
      description         <- Gen.option(smartStringGen)
      shortName           <- Gen.option(smartStringGen)
      fields              <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      showReference       <- PrimitiveGen.booleanGen
      pdf                 <- Gen.option(pdfContextGen)
      instructionPdf      <- Gen.option(pdfContextGen)
      displayFeedbackLink <- PrimitiveGen.booleanGen
      nonPIITitle         <- Gen.option(smartStringGen)
      showBanner          <- PrimitiveGen.booleanGen
    } yield AcknowledgementSection(
      title,
      description,
      shortName,
      fields.toList,
      showReference,
      pdf,
      instructionPdf,
      displayFeedbackLink,
      nonPIITitle,
      showBanner
    )

  def pdfContextGen: Gen[PdfCtx] =
    for {
      header <- Gen.option(smartStringGen)
      footer <- Gen.option(smartStringGen)
    } yield PdfCtx(header, footer, None, None)

  def declarationSectionGen: Gen[DeclarationSection] =
    for {
      title         <- smartStringGen
      caption       <- Gen.option(smartStringGen)
      noPIITitle    <- Gen.option(smartStringGen)
      description   <- Gen.option(smartStringGen)
      shortName     <- Gen.option(smartStringGen)
      continueLabel <- Gen.option(smartStringGen)
      fields        <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      includeIf     <- Gen.option(IncludeIfGen.includeIfGen)
    } yield DeclarationSection(
      title,
      caption,
      noPIITitle,
      description,
      shortName,
      continueLabel,
      fields.toList,
      includeIf
    )

  def checkYourAnswerPageGen: Gen[CheckYourAnswersPage] =
    for {
      title            <- Gen.option(smartStringGen)
      caption          <- Gen.option(smartStringGen)
      updateTitle      <- smartStringGen
      noPIITitle       <- Gen.option(smartStringGen)
      noPIIUpdateTitle <- Gen.option(smartStringGen)
      header           <- Gen.option(smartStringGen)
      footer           <- Gen.option(smartStringGen)
      continueLabel    <- Gen.option(smartStringGen)
      presentationHint <- Gen.option(PresentationHintGen.presentationHintGen)
      removeItemIf     <- Gen.option(RemoveItemIfGen.removeItemIfGen)
      fields           <- Gen.option(PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen()))
      displayWidth     <- Gen.option(Gen.oneOf(LayoutDisplayWidth.values.toSeq))
    } yield CheckYourAnswersPage(
      title,
      caption,
      updateTitle,
      noPIITitle,
      noPIIUpdateTitle,
      header,
      footer,
      continueLabel,
      presentationHint,
      removeItemIf,
      fields,
      displayWidth,
      None
    )

  def pageGen: Gen[Page] =
    for {
      title            <- smartStringGen
      id               <- Gen.option(PageIdGen.pageIdGen)
      noPIITitle       <- Gen.option(smartStringGen)
      description      <- Gen.option(smartStringGen)
      caption          <- Gen.option(smartStringGen)
      shortName        <- Gen.option(smartStringGen)
      includeIf        <- Gen.option(IncludeIfGen.includeIfGen)
      fields           <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      continueLabel    <- Gen.option(smartStringGen)
      continueIf       <- Gen.option(ContinueIfGen.continueIfGen)
      instruction      <- Gen.option(InstructionGen.instructionGen)
      presentationHint <- Gen.option(PresentationHintGen.presentationHintGen)
      confirmation     <- Gen.option(ConfirmationGen.confirmationGen)
      redirects        <- Gen.option(RedirectGen.redirectGen)
      removeItemIf     <- Gen.option(RemoveItemIfGen.removeItemIfGen)
    } yield Page(
      title,
      id,
      noPIITitle,
      description,
      caption,
      shortName,
      includeIf,
      fields.toList,
      continueLabel,
      continueIf,
      instruction,
      presentationHint,
      None,
      confirmation,
      redirects.map(NonEmptyList.one),
      None,
      removeItemIf
    )

  def nonRepeatingPageSectionGen: Gen[Section.NonRepeatingPage] = pageGen.map(Section.NonRepeatingPage)

  def repeatingPageSectionGen: Gen[Section.RepeatingPage] =
    for {
      page    <- pageGen
      repeats <- ExprGen.exprGen()
    } yield Section.RepeatingPage(page, repeats)

  def addToListSectionGen: Gen[Section.AddToList] =
    for {
      title                 <- smartStringGen
      caption               <- Gen.option(smartStringGen)
      noPIITitle            <- Gen.option(smartStringGen)
      description           <- smartStringGen
      summaryDescription    <- smartStringGen
      shortName             <- smartStringGen
      summaryName           <- smartStringGen
      includeIf             <- Gen.option(IncludeIfGen.includeIfGen)
      pages                 <- PrimitiveGen.oneOrMoreGen(pageGen)
      repeatsUntil          <- Gen.option(IncludeIfGen.includeIfGen)
      repeatsWhile          <- Gen.option(IncludeIfGen.includeIfGen)
      repeaterContinueLabel <- Gen.option(smartStringGen)
      formComponent         <- FormComponentGen.formComponentGen(0)
      choice                <- ComponentTypeGen.choiceGen
      instruction           <- Gen.option(InstructionGen.instructionGen)
      presentationHint      <- Gen.option(PresentationHintGen.presentationHintGen)
      infoMessage           <- Gen.option(smartStringGen)
      errorMessage          <- Gen.option(smartStringGen)
      maybePageId           <- Gen.option(PageIdGen.pageIdGen)
      cyaPage               <- Gen.option(checkYourAnswerPageGen)
      descriptionTotal      <- smartStringGen
    } yield Section
      .AddToList(
        title,
        caption,
        noPIITitle,
        AtlDescription.SmartStringBased(description),
        summaryDescription,
        shortName,
        summaryName,
        includeIf,
        pages,
        repeatsUntil,
        repeatsWhile,
        repeaterContinueLabel,
        formComponent.copy(`type` = choice),
        instruction,
        presentationHint,
        infoMessage,
        errorMessage,
        pageIdToDisplayAfterRemove = maybePageId,
        cyaPage = cyaPage,
        descriptionTotal = Some(AtlDescription.KeyValueBased(descriptionTotal, descriptionTotal)),
        displayWidth = None
      )

  def sectionGen: Gen[Section] = Gen.oneOf(nonRepeatingPageSectionGen, repeatingPageSectionGen, addToListSectionGen)
}

object SectionGen extends SectionGen
