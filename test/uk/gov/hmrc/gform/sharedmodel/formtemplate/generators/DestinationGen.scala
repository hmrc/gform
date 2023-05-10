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
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.form.FormStatus
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, Destination, DestinationId, DestinationIncludeIf, ProjectId }
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierPersonalisationFieldId, NotifierTemplateId }

trait DestinationGen {
  def destinationIdGen: Gen[DestinationId] =
    for {
      f <- Gen.alphaChar
      r <- Gen.alphaNumStr
    } yield DestinationId(f.toString + r)

  def dmsFormIdGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(_.take(12))
  def classificationTypeGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def customerIdGen: Gen[Expr] = ExprGen.exprGen()
  def businessAreaGen: Gen[String] = PrimitiveGen.nonEmptyAlphaNumStrGen
  def signatureGen: Gen[String] = PrimitiveGen.nonEmptyAsciiPrintableString
  def projectIdGen: Gen[ProjectId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(ProjectId(_))

  def hmrcDmsGen: Gen[Destination.HmrcDms] =
    for {
      id                    <- destinationIdGen
      dmsFormId             <- dmsFormIdGen
      customerId            <- customerIdGen
      classificationType    <- classificationTypeGen
      businessArea          <- businessAreaGen
      includeIf             <- includeIfGen()
      failOnError           <- PrimitiveGen.booleanGen
      formdataXml           <- PrimitiveGen.booleanGen
      backscan              <- Gen.option(PrimitiveGen.booleanGen)
      includeInstructionPdf <- PrimitiveGen.booleanGen
    } yield Destination
      .HmrcDms(
        id,
        dmsFormId,
        customerId,
        classificationType,
        businessArea,
        includeIf,
        failOnError,
        Some(DataOutputFormat.XML),
        formdataXml,
        backscan,
        includeInstructionPdf
      )

  def submissionConsolidatorGen: Gen[Destination.SubmissionConsolidator] =
    for {
      id          <- destinationIdGen
      projectId   <- projectIdGen
      customerId  <- customerIdGen
      includeIf   <- includeIfGen()
      failOnError <- PrimitiveGen.booleanGen
      formData <- Gen.option(for {
                    id    <- Gen.alphaStr.suchThat(_.nonEmpty)
                    value <- Gen.alphaNumStr.suchThat(_.nonEmpty)
                  } yield s"""[{"id": "$id", "value": "$value"}]""")
    } yield Destination
      .SubmissionConsolidator(id, projectId, customerId, formData, includeIf, failOnError)

  def hmrcDmsGen(
    includeIf: Option[DestinationIncludeIf] = None,
    failOnError: Option[Boolean] = None
  ): Gen[Destination.HmrcDms] =
    hmrcDmsGen.map { g =>
      g.copy(
        includeIf = includeIf.getOrElse(HandlebarValue("true")),
        failOnError = failOnError.getOrElse(g.failOnError)
      )
    }

  def handlebarsHttpApiGen: Gen[Destination.HandlebarsHttpApi] =
    for {
      id                  <- destinationIdGen
      profile             <- ProfileNameGen.profileGen
      uri                 <- PrimitiveGen.urlContextPathGen
      method              <- HttpMethodGen.httpMethodGen
      payload             <- Gen.option(PrimitiveGen.nonEmptyAlphaNumStrGen).map(_.map(s => s""""$s""""))
      payloadType         <- TemplateTypeGen.templateTypeGen
      includeIf           <- includeIfGen()
      failOnError         <- PrimitiveGen.booleanGen
      multiRequestPayload <- PrimitiveGen.booleanGen
    } yield Destination.HandlebarsHttpApi(
      id,
      profile,
      uri,
      method,
      payload,
      None,
      payloadType,
      includeIf,
      failOnError,
      multiRequestPayload
    )

  def handlebarsHttpApiGen(
    includeIf: Option[DestinationIncludeIf] = None,
    failOnError: Option[Boolean] = None
  ): Gen[Destination.HandlebarsHttpApi] =
    handlebarsHttpApiGen.map { g =>
      g.copy(
        includeIf = includeIf.getOrElse(HandlebarValue("true")),
        failOnError = failOnError.getOrElse(g.failOnError)
      )
    }

  def compositeGen: Gen[Destination.Composite] =
    for {
      id           <- destinationIdGen
      includeIf    <- includeIfGen()
      destinations <- PrimitiveGen.oneOrMoreGen(singularDestinationGen)
    } yield Destination.Composite(id, includeIf, destinations)

  def stateTransitionGen: Gen[Destination.StateTransition] =
    for {
      id            <- destinationIdGen
      includeIf     <- includeIfGen()
      failOnError   <- PrimitiveGen.booleanGen
      requiredState <- FormGen.formStatusGen
    } yield Destination.StateTransition(id, requiredState, includeIf, failOnError)

  def stateTransitionGen(
    includeIf: Option[DestinationIncludeIf] = None,
    failOnError: Option[Boolean] = None,
    requiredState: Option[FormStatus] = None
  ): Gen[Destination.StateTransition] =
    stateTransitionGen.map { st =>
      st.copy(
        requiredState = requiredState.getOrElse(st.requiredState),
        includeIf = includeIf.getOrElse(HandlebarValue("true")),
        failOnError = failOnError.getOrElse(st.failOnError)
      )
    }

  def logGen: Gen[Destination.Log] =
    for {
      id <- destinationIdGen
    } yield Destination.Log(id)

  def emailGen: Gen[Destination.Email] =
    for {
      id              <- destinationIdGen
      emailTemplateId <- PrimitiveGen.nonEmptyAlphaNumStrGen.map(NotifierTemplateId(_))
      includeIf       <- includeIfGen()
      to              <- FormComponentGen.formComponentIdGen
      personalisation <- PrimitiveGen.possiblyEmptyMapGen(
                           PrimitiveGen.nonEmptyAlphaNumStrGen.map(NotifierPersonalisationFieldId(_)),
                           FormComponentGen.formComponentIdGen
                         )
      failOnError <- PrimitiveGen.booleanGen
    } yield Destination.Email(
      id,
      EmailVerifierService.Notify(emailTemplateId, None),
      includeIf,
      failOnError,
      to,
      personalisation
    )

  def singularDestinationGen: Gen[Destination] =
    Gen.oneOf(hmrcDmsGen, handlebarsHttpApiGen, stateTransitionGen, logGen, emailGen, submissionConsolidatorGen)

  def destinationGen: Gen[Destination] = Gen.frequency(10 -> singularDestinationGen, 1 -> compositeGen)

  def destinationWithFixedIdGen(id: DestinationId): Gen[Destination] = hmrcDmsGen.map(_.copy(id = id))

  def includeIfGen(): Gen[DestinationIncludeIf] =
    Gen.oneOf(Gen.alphaNumStr, Gen.const("true"), Gen.const("false")).map(HandlebarValue)
}

object DestinationGen extends DestinationGen
