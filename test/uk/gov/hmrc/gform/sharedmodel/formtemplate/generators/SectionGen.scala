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
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

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
      title       <- PrimitiveGen.nonEmptyAlphaNumStrGen
      shortName   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      fields      <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      identifiers <- PrimitiveGen.oneOrMoreGen(identifierRecipeGen)
      verifiers   <- PrimitiveGen.zeroOrMoreGen(verifierRecipeGen)
    } yield
      EnrolmentSection(toLocalisedString(title), toLocalisedString(shortName), fields.toList, identifiers, verifiers)

  def acknowledgementSectionGen: Gen[AcknowledgementSection] =
    for {
      title       <- PrimitiveGen.nonEmptyAlphaNumStrGen
      description <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      shortName   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      fields      <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
    } yield
      AcknowledgementSection(
        toLocalisedString(title),
        toLocalisedString(description),
        toLocalisedString(shortName),
        fields.toList)

  def declarationSectionGen: Gen[DeclarationSection] =
    for {
      title       <- PrimitiveGen.nonEmptyAlphaNumStrGen
      description <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      shortName   <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      fields      <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
    } yield
      DeclarationSection(
        toLocalisedString(title),
        toLocalisedString(description),
        toLocalisedString(shortName),
        fields.toList)

  def sectionGen: Gen[Section] =
    for {
      title             <- PrimitiveGen.nonEmptyAlphaNumStrGen
      description       <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      progressIndicator <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      shortName         <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      includeIf         <- Gen.option(IncludeIfGen.includeIfGen)
      repeatsMax        <- Gen.option(FormatExprGen.textExpressionGen)
      repeatMin         <- Gen.option(FormatExprGen.textExpressionGen)
      validators        <- Gen.option(ValidatorGen.validatorGen)
      fields            <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      continueLabel     <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      continueIf        <- Gen.option(ContinueIfGen.continueIfGen)
    } yield
      Section(
        toLocalisedString(title),
        toLocalisedString(description),
        toLocalisedString(progressIndicator),
        toLocalisedString(shortName),
        includeIf,
        repeatsMax,
        repeatMin,
        validators,
        fields.toList,
        toLocalisedString(continueLabel),
        continueIf
      )
}

object SectionGen extends SectionGen
