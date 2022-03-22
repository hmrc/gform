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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators
import java.time.LocalTime

import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DisplayWidth.DisplayWidth
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait ComponentTypeGen {

  def displayWidthGen: Gen[DisplayWidth] = Gen.oneOf(DisplayWidth.values.toSeq)
  def rowsGen: Gen[Int] = Gen.posNum[Int]

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
      rows         <- rowsGen
    } yield TextArea(constraint, value, displayWidth, rows)

  def dateGen: Gen[Date] =
    for {
      constraintType <- FormatExprGen.dateConstraintTypeGen
      offset         <- OffsetGen.offsetGen
      value          <- Gen.option(DateValueGen.dateValueGen)
    } yield Date(constraintType, offset, value)

  def addressGen: Gen[Address] = PrimitiveGen.booleanGen.map(Address(_))

  def choiceTypeGen: Gen[ChoiceType] = Gen.oneOf(Radio, Checkbox, YesNo)

  def orientationGen: Gen[Orientation] = Gen.oneOf(Vertical, Horizontal)

  def choiceGen: Gen[Choice] =
    for {
      tpe         <- choiceTypeGen
      options     <- PrimitiveGen.oneOrMoreGen(SmartStringGen.smartStringGen)
      orientation <- orientationGen
      selections  <- PrimitiveGen.zeroOrMoreGen(Gen.posNum[Int])
      hints       <- Gen.option(PrimitiveGen.oneOrMoreGen(SmartStringGen.smartStringGen))
      helpText    <- Gen.option(PrimitiveGen.oneOrMoreGen(SmartStringGen.smartStringGen))
    } yield Choice(
      tpe,
      options,
      orientation,
      selections,
      hints,
      helpText,
      None,
      LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
      None
    )

  def revealingChoiceElementGen: Gen[RevealingChoiceElement] =
    for {
      choice          <- SmartStringGen.smartStringGen
      revealingFields <- PrimitiveGen.zeroOrMoreGen(FormComponentGen.formComponentGen(1))
      hint            <- Gen.option(SmartStringGen.smartStringGen)
      selected        <- PrimitiveGen.booleanGen
    } yield RevealingChoiceElement(choice, revealingFields, hint, selected)

  def revealingChoiceGen: Gen[RevealingChoice] =
    for {
      revealingChoiceElements <- PrimitiveGen.oneOrMoreGen(revealingChoiceElementGen)
      multivalue              <- PrimitiveGen.booleanGen
    } yield RevealingChoice(revealingChoiceElements, multivalue)

  def hmrcTaxPeriodGen: Gen[HmrcTaxPeriod] =
    for {
      idType     <- IdTypeGen.idTypeGen
      expr       <- ExprGen.exprGen()
      regimeType <- RegimeTypeGen.regimeTypeGen
    } yield HmrcTaxPeriod(idType, expr, regimeType)

  def groupGen(maxDepth: Int): Gen[Group] =
    for {
      fields               <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen(maxDepth - 1))
      repeatsMax           <- Gen.option(Gen.posNum[Int])
      repeatsMin           <- Gen.option(Gen.posNum[Int])
      repeatLabel          <- Gen.option(SmartStringGen.smartStringGen)
      repeatAddAnotherText <- Gen.option(SmartStringGen.smartStringGen)
    } yield Group(fields.toList, repeatsMax, repeatsMin, repeatLabel, repeatAddAnotherText)

  def infoTypeGen: Gen[InfoType] = Gen.oneOf(StandardInfo, LongInfo, ImportantInfo, BannerInfo, NoFormat)

  def informationMessageGen: Gen[InformationMessage] =
    for {
      infoType <- infoTypeGen
      infoText <- SmartStringGen.smartStringGen
    } yield InformationMessage(infoType, infoText)

  def fileUploadGen: Gen[FileUpload] =
    Gen.oneOf(FileUploadProvider.FileUploadFrontend, FileUploadProvider.Upscan).map(FileUpload(_))

  def localTimeGen(baseTime: LocalTime): Gen[LocalTime] =
    Gen.oneOf(
      Seq(
        baseTime,
        baseTime.plusHours(2),
        baseTime.plusHours(4),
        baseTime.plusHours(6),
        baseTime.plusHours(8),
        baseTime.plusHours(10)
      )
    )

  def startTimeGen: Gen[StartTime] =
    for {
      localTime <- localTimeGen(LocalTime.MIN)
    } yield StartTime(localTime)

  def endTimeGen: Gen[EndTime] =
    for {
      localTime <- localTimeGen(LocalTime.NOON)
    } yield EndTime(localTime)

  def rangeGen: Gen[Range] =
    for {
      startTime <- startTimeGen
      endTime   <- endTimeGen
    } yield Range(startTime, endTime)

  def intervalMinsGen: Gen[IntervalMins] =
    for {
      intervalMins <- Gen.choose(1, 60)
    } yield IntervalMins(intervalMins)

  def timeGen: Gen[Time] =
    for {
      ranges       <- Gen.listOf(rangeGen)
      intervalMins <- intervalMinsGen
    } yield Time(ranges, intervalMins)

  def componentTypeGen(maxDepth: Int = 3): Gen[ComponentType] =
    if (maxDepth <= 1)
      Gen.oneOf(
        choiceGen,
        dateGen,
        textGen,
        textAreaGen,
        addressGen,
        informationMessageGen,
        fileUploadGen,
        hmrcTaxPeriodGen,
        revealingChoiceGen,
        timeGen
      )
    else
      Gen.oneOf(
        textGen,
        textAreaGen,
        dateGen,
        addressGen,
        choiceGen,
        informationMessageGen,
        fileUploadGen,
        hmrcTaxPeriodGen,
        revealingChoiceGen,
        timeGen,
        groupGen(maxDepth)
      )
}

object ComponentTypeGen extends ComponentTypeGen
