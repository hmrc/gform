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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import cats.syntax.either._
import cats.syntax.option._
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import UploadableConditioning._
import cats.data.NonEmptyList
import JsonUtils.nelFormat
import uk.gov.hmrc.gform.sharedmodel.form.FormStatus
import uk.gov.hmrc.gform.sharedmodel.notifier.{ NotifierPersonalisationFieldId, NotifierTemplateId }

sealed trait DestinationWithCustomerId {
  def customerId(): TextExpression
}

sealed trait Destination extends Product with Serializable {
  def id: DestinationId
  def includeIf: String
  def failOnError: Boolean
}

object Destination {
  case class HmrcDms(
    id: DestinationId,
    dmsFormId: String,
    customerId: TextExpression,
    classificationType: String,
    businessArea: String,
    includeIf: String,
    failOnError: Boolean,
    roboticsXml: Boolean)
      extends Destination with DestinationWithCustomerId {
    def toDeprecatedDmsSubmission: Destinations.DmsSubmission = Destinations.DmsSubmission(
      dmsFormId,
      customerId,
      classificationType,
      businessArea,
      includeRoboticsXml = Some(roboticsXml)
    )
  }

  case class HandlebarsHttpApi(
    id: DestinationId,
    profile: ProfileName,
    uri: String,
    method: HttpMethod,
    payload: Option[String],
    payloadType: TemplateType,
    includeIf: String,
    failOnError: Boolean)
      extends Destination

  case class Composite(id: DestinationId, includeIf: String, destinations: NonEmptyList[Destination])
      extends Destination {
    val failOnError: Boolean = false
  }

  case class StateTransition(id: DestinationId, requiredState: FormStatus, includeIf: String, failOnError: Boolean)
      extends Destination

  case class Log(id: DestinationId) extends Destination {
    val failOnError: Boolean = false
    val includeIf: String = true.toString
  }

  case class Email(
    id: DestinationId,
    emailTemplateId: NotifierTemplateId,
    includeIf: String,
    failOnError: Boolean,
    to: FormComponentId,
    personalisation: Map[NotifierPersonalisationFieldId, FormComponentId])
      extends Destination

  val typeDiscriminatorFieldName: String = "type"
  val hmrcDms: String = "hmrcDms"
  val handlebarsHttpApi: String = "handlebarsHttpApi"
  val composite: String = "composite"
  val stateTransition: String = "stateTransition"
  val log: String = "log"
  val email: String = "email"

  private implicit def nonEmptyListOfDestinationsFormat: OFormat[NonEmptyList[Destination]] =
    derived.oformat[NonEmptyList[Destination]]

  implicit def format: OFormat[Destination] = {
    implicit val personalisationReads =
      JsonUtils.formatMap[NotifierPersonalisationFieldId, FormComponentId](NotifierPersonalisationFieldId(_), _.value)

    implicit def d: OFormat[Destination] = derived.oformat[Destination]

    OFormatWithTemplateReadFallback(
      ADTFormat.adtRead[Destination](
        typeDiscriminatorFieldName,
        hmrcDms           -> UploadableHmrcDmsDestination.reads,
        handlebarsHttpApi -> UploadableHandlebarsHttpApiDestination.reads,
        composite         -> UploadableCompositeDestination.reads,
        stateTransition   -> UploadableStateTransitionDestination.reads,
        log               -> UploadableLogDestination.reads,
        email             -> UploadableEmailDestination.reads
      ))
  }
}

case class UploadableHmrcDmsDestination(
  id: DestinationId,
  dmsFormId: String,
  customerId: TextExpression,
  classificationType: String,
  businessArea: String,
  convertSingleQuotes: Option[Boolean],
  includeIf: Option[String] = None,
  failOnError: Option[Boolean] = None) {

  def toHmrcDmsDestination: Either[String, Destination.HmrcDms] =
    for {
      cii <- addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, includeIf))
    } yield
      Destination.HmrcDms(
        id,
        dmsFormId,
        customerId,
        classificationType,
        businessArea,
        cii.getOrElse(true.toString),
        failOnError.getOrElse(true),
        false)
}

object UploadableHmrcDmsDestination {
  implicit val reads: Reads[Destination.HmrcDms] = new Reads[Destination.HmrcDms] {
    private val d: Reads[UploadableHmrcDmsDestination] = derived.reads[UploadableHmrcDmsDestination]
    override def reads(json: JsValue): JsResult[Destination.HmrcDms] =
      d.reads(json).flatMap(_.toHmrcDmsDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableCompositeDestination(
  id: DestinationId,
  convertSingleQuotes: Option[Boolean],
  includeIf: Option[String],
  destinations: NonEmptyList[Destination]) {
  def toCompositeDestination: Either[String, Destination.Composite] =
    for {
      cvii <- addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, includeIf))
    } yield Destination.Composite(id, cvii.getOrElse(true.toString), destinations)
}

object UploadableCompositeDestination {
  implicit val reads: Reads[Destination.Composite] = new Reads[Destination.Composite] {
    private val d: Reads[UploadableCompositeDestination] = derived.reads[UploadableCompositeDestination]
    override def reads(json: JsValue): JsResult[Destination.Composite] =
      d.reads(json).flatMap(_.toCompositeDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableStateTransitionDestination(
  id: DestinationId,
  requiredState: String,
  convertSingleQuotes: Option[Boolean],
  includeIf: Option[String],
  failOnError: Option[Boolean] = None) {
  def toStateTransitionDestination: Either[String, Destination.StateTransition] =
    for {
      cvii <- addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, includeIf))
      rs <- addErrorInfo(id, "requiredState")(FormStatus
             .unapply(requiredState)
             .fold[Either[String, FormStatus]](
               s"Invalid requiredState: '$requiredState'. Allowed states are: ${FormStatus.all}".asLeft) { _.asRight })
    } yield Destination.StateTransition(id, rs, cvii.getOrElse(true.toString), failOnError.getOrElse(true))
}

object UploadableStateTransitionDestination {
  implicit val reads: Reads[Destination.StateTransition] = new Reads[Destination.StateTransition] {
    private val d: Reads[UploadableStateTransitionDestination] = derived.reads[UploadableStateTransitionDestination]
    override def reads(json: JsValue): JsResult[Destination.StateTransition] =
      d.reads(json).flatMap(_.toStateTransitionDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableHandlebarsHttpApiDestination(
  id: DestinationId,
  profile: ProfileName,
  uri: String,
  method: HttpMethod,
  payload: Option[String],
  payloadType: Option[TemplateType],
  convertSingleQuotes: Option[Boolean],
  includeIf: Option[String],
  failOnError: Option[Boolean]) {

  def toHandlebarsHttpApiDestination: Either[String, Destination.HandlebarsHttpApi] =
    for {
      cvp   <- addErrorInfo(id, "payload")(conditionAndValidate(convertSingleQuotes, payload))
      cvii  <- addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, includeIf))
      cvuri <- addErrorInfo(id, "uri")(condition(convertSingleQuotes, uri))
    } yield
      Destination
        .HandlebarsHttpApi(
          id,
          profile,
          cvuri,
          method,
          cvp,
          payloadType.getOrElse(TemplateType.JSON),
          cvii.getOrElse(true.toString),
          failOnError.getOrElse(true))
}

object UploadableHandlebarsHttpApiDestination {
  implicit val reads: Reads[Destination.HandlebarsHttpApi] = new Reads[Destination.HandlebarsHttpApi] {
    private val d = derived.reads[UploadableHandlebarsHttpApiDestination]
    override def reads(json: JsValue): JsResult[Destination.HandlebarsHttpApi] =
      d.reads(json).flatMap(_.toHandlebarsHttpApiDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableLogDestination(id: DestinationId) {
  def toLogDestination: Either[String, Destination.Log] = Right(Destination.Log(id))
}

object UploadableLogDestination {
  implicit val reads: Reads[Destination.Log] = new Reads[Destination.Log] {
    private val d: Reads[UploadableLogDestination] = derived.reads[UploadableLogDestination]
    override def reads(json: JsValue): JsResult[Destination.Log] =
      d.reads(json).flatMap(_.toLogDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableEmailDestination(
  id: DestinationId,
  emailTemplateId: NotifierTemplateId,
  convertSingleQuotes: Option[Boolean],
  includeIf: Option[String],
  failOnError: Option[Boolean],
  to: FormComponentId,
  personalisation: Map[NotifierPersonalisationFieldId, FormComponentId]) {
  def toEmailDestination: Either[String, Destination.Email] =
    for {
      cvii <- addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, includeIf))
    } yield
      Destination
        .Email(id, emailTemplateId, cvii.getOrElse(true.toString), failOnError.getOrElse(true), to, personalisation)
}

object UploadableEmailDestination {
  private implicit val personalisationReads =
    JsonUtils.formatMap[NotifierPersonalisationFieldId, FormComponentId](NotifierPersonalisationFieldId(_), _.value)

  implicit val reads: Reads[Destination.Email] = new Reads[Destination.Email] {
    private val d: Reads[UploadableEmailDestination] = derived.reads[UploadableEmailDestination]
    override def reads(json: JsValue): JsResult[Destination.Email] =
      d.reads(json).flatMap(_.toEmailDestination.fold(JsError(_), JsSuccess(_)))
  }
}

object UploadableConditioning {
  def addErrorInfo[T](destinationId: DestinationId, field: String)(result: Either[String, T]): Either[String, T] =
    result.leftMap(e => s"${destinationId.id}/$field $e")

  def condition(convertSingleQuotes: Option[Boolean], os: Option[String]): Either[String, Option[String]] = os match {
    case None    => Right(os)
    case Some(s) => condition(convertSingleQuotes, s).map(_.some)
  }

  def condition(convertSingleQuotes: Option[Boolean], s: String): Either[String, String] =
    if (convertSingleQuotes.getOrElse(false)) SingleQuoteReplacementLexer(s)
    else Right(s)

  private def validate(s: String): Either[String, String] = Right(s)

  def conditionAndValidate(convertSingleQuotes: Option[Boolean], s: String): Either[String, String] =
    condition(convertSingleQuotes, s).flatMap(validate)

  def conditionAndValidate(convertSingleQuotes: Option[Boolean], os: Option[String]): Either[String, Option[String]] =
    os map (conditionAndValidate(convertSingleQuotes, _) map (_.some)) getOrElse Right(None)
}
