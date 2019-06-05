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
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait ComponentTypeGen {
  def listToLocalisedString(stringList: NonEmptyList[String]): NonEmptyList[LocalisedString] =
    stringList.map(s => toLocalisedString(s))

  def toLocalisedString(string: String): LocalisedString =
    LocalisedString(Map(LangADT.En -> string))

  def toOptLocalisedString(optionalString: Option[String]): Option[LocalisedString] =
    optionalString.map(s => toLocalisedString(s))

  def optListToLocalisedString(optionalStringList: Option[List[String]]): Option[List[LocalisedString]] =
    optionalStringList.map(stringList => stringList.map(string => toLocalisedString(string)))

  def displayWidthGen: Gen[DisplayWidth] = Gen.oneOf(DisplayWidth.values.toSeq)

  def textGen: Gen[Text] =
    for {
      constraint   <- FormatExprGen.textConstraintGen
      value        <- ExprGen.exprGen()
      displayWidth <- displayWidthGen
    } yield Text(constraint, value, displayWidth)

  def textAreaGen: Gen[TextArea] =
    for {
      constraint   <- FormatExprGen.textConstraintGen
      value        <- ExprGen.exprGen()
      displayWidth <- displayWidthGen
    } yield TextArea(constraint, value, displayWidth)

  def ukSortCodeGen: Gen[UkSortCode] = ExprGen.exprGen().map(UkSortCode(_))

  def dateGen: Gen[Date] =
    for {
      constraintType <- FormatExprGen.dateConstraintTypeGen
      offset         <- OffsetGen.offsetGen
      value          <- Gen.option(DateValueGen.dateValueGen)
    } yield Date(constraintType, offset, value)

  def addressGen: Gen[Address] = PrimitiveGen.booleanGen.map(Address(_))

  def choiceTypeGen: Gen[ChoiceType] = Gen.oneOf(Radio, Checkbox, YesNo, Inline)

  def orientationGen: Gen[Orientation] = Gen.oneOf(Vertical, Horizontal)

  def choiceGen: Gen[Choice] =
    for {
      tpe         <- choiceTypeGen
      options     <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(NonEmptyList.of(_))
      orientation <- orientationGen
      selections  <- PrimitiveGen.zeroOrMoreGen(Gen.posNum[Int])
      helpText    <- Gen.option(PrimitiveGen.zeroOrMoreGen(PrimitiveGen.nonEmptyAlphaNumStrGen))
    } yield Choice(tpe, listToLocalisedString(options), orientation, selections, optListToLocalisedString(helpText))

  def revealingChoiceElementGen: Gen[RevealingChoiceElement] =
    for {
      choice   <- PrimitiveGen.nonEmptyAlphaNumStrGen
      fields   <- PrimitiveGen.zeroOrMoreGen(FormComponentGen.formComponentGen(3))
      selected <- PrimitiveGen.booleanGen
    } yield RevealingChoiceElement(toLocalisedString(choice), fields, selected)

  def revealingChoiceGen: Gen[RevealingChoice] =
    for {
      revealingChoiceElements <- PrimitiveGen.oneOrMoreGen(revealingChoiceElementGen)
    } yield RevealingChoice(revealingChoiceElements)

  def hmrcTaxPeriodGen: Gen[HmrcTaxPeriod] =
    for {
      idType     <- IdTypeGen.idTypeGen
      expr       <- ExprGen.exprGen()
      regimeType <- RegimeTypeGen.regimeTypeGen
    } yield HmrcTaxPeriod(idType, expr, regimeType)

  def groupGen(maxDepth: Int): Gen[Group] =
    for {
      fields               <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen(maxDepth - 1))
      orientation          <- orientationGen
      repeatsMax           <- Gen.option(Gen.posNum[Int])
      repeatsMin           <- Gen.option(Gen.posNum[Int])
      repeatLabel          <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
      repeatAddAnotherText <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen)
    } yield
      Group(
        fields.toList,
        orientation,
        repeatsMax,
        repeatsMin,
        toOptLocalisedString(repeatLabel),
        toOptLocalisedString(repeatAddAnotherText))

  def infoTypeGen: Gen[InfoType] = Gen.oneOf(StandardInfo, LongInfo, ImportantInfo, BannerInfo, NoFormat)

  def informationMessageGen: Gen[InformationMessage] =
    for {
      infoType <- infoTypeGen
      infoText <- PrimitiveGen.nonEmptyAlphaNumStrGen
    } yield InformationMessage(infoType, toLocalisedString(infoText))

  def fileUploadGen: Gen[FileUpload] = Gen.const(FileUpload())

  def componentTypeGen(maxDepth: Int = 3): Gen[ComponentType] =
    if (maxDepth <= 1)
      Gen.oneOf(
        choiceGen,
        dateGen,
        textGen,
        textAreaGen,
        ukSortCodeGen,
        addressGen,
        informationMessageGen,
        fileUploadGen,
        hmrcTaxPeriodGen,
        revealingChoiceGen
      )
    else
      Gen.oneOf(
        textGen,
        textAreaGen,
        ukSortCodeGen,
        dateGen,
        addressGen,
        choiceGen,
        informationMessageGen,
        fileUploadGen,
        hmrcTaxPeriodGen,
        revealingChoiceGen,
        groupGen(maxDepth)
      )
}

object ComponentTypeGen extends ComponentTypeGen
