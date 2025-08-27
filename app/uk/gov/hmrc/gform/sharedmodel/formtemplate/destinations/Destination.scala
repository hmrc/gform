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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations

import play.api.libs.json._
import cats.syntax.either._
import cats.syntax.option._
import julienrf.json.derived
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import UploadableConditioning._
import cats.data.NonEmptyList
import JsonUtils.nelFormat
import uk.gov.hmrc.gform.core.parsers.BooleanExprParser
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.{ HandlebarValue, IncludeIfValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.SubmissionConsolidator
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierPersonalisationFieldId
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination

sealed trait DestinationWithCustomerId {
  def customerId(): Expr
}

sealed trait DestinationWithTaxpayerId extends Destination {
  def taxpayerId(): Expr
}

sealed trait DestinationWithPaymentReference extends Destination {
  def paymentReference: Expr
  def nino: Option[Expr]
  def utr: Option[Expr]
  def postalCode: Option[Expr]
}

sealed trait DestinationWithPegaCaseId extends Destination {
  def caseId: Expr
}

sealed trait DestinationIncludeIf extends Product with Serializable

object DestinationIncludeIf {
  case class HandlebarValue(value: String) extends DestinationIncludeIf
  case class IncludeIfValue(value: IncludeIf) extends DestinationIncludeIf
  private val templateReads: Reads[DestinationIncludeIf] = Reads {
    case JsString(exprAsStr) =>
      BooleanExprParser.validate(exprAsStr) fold (_ => JsSuccess(HandlebarValue(exprAsStr)), expr =>
        JsSuccess(IncludeIfValue(IncludeIf(expr))))
    case JsNull => JsSuccess(HandlebarValue(true.toString))
    case _      => JsError("Unsupported includeIf expression in destination")
  }

  implicit val format: OFormat[DestinationIncludeIf] = OFormatWithTemplateReadFallback(templateReads)
}

sealed trait Destination extends Product with Serializable {
  def id: DestinationId
  def includeIf: DestinationIncludeIf
  def failOnError: Boolean
}

object Destination {
  case class HmrcDms(
    id: DestinationId,
    dmsFormId: String,
    customerId: Expr,
    classificationType: String,
    businessArea: String,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    dataOutputFormat: Option[DataOutputFormat],
    formdataXml: Boolean,
    backscan: Option[Boolean],
    instructionPdfFields: Option[InstructionPdfFields],
    convertSingleQuotes: Option[Boolean],
    payload: Option[String],
    payloadType: TemplateType
  ) extends Destination with DestinationWithCustomerId

  case class DataStore(
    id: DestinationId,
    routing: SdesDestination,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    formId: FormId,
    version: String,
    taxpayerId: Expr,
    regime: String,
    includeSessionInfo: Boolean,
    handlebarPayload: Boolean,
    formDataPayload: Boolean,
    convertSingleQuotes: Option[Boolean],
    payload: Option[String],
    validateHandlebarPayload: Boolean,
    jsonSchema: Option[JsValue]
  ) extends Destination with DestinationWithTaxpayerId

  case class InfoArchive(
    id: DestinationId,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    formId: FormId,
    paymentReference: Expr,
    nino: Option[Expr],
    utr: Option[Expr],
    postalCode: Option[Expr]
  ) extends Destination with DestinationWithPaymentReference

  case class HandlebarsHttpApi(
    id: DestinationId,
    profile: ProfileName,
    uri: String,
    method: HttpMethod,
    payload: Option[String],
    payloadType: TemplateType,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    multiRequestPayload: Boolean,
    convertSingleQuotes: Option[Boolean]
  ) extends Destination

  case class Composite(id: DestinationId, includeIf: DestinationIncludeIf, destinations: NonEmptyList[Destination])
      extends Destination {
    val failOnError: Boolean = false
  }

  case class StateTransition(
    id: DestinationId,
    requiredState: FormStatus,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean
  ) extends Destination

  case class Log(id: DestinationId) extends Destination {
    val failOnError: Boolean = false
    val includeIf: DestinationIncludeIf = DestinationIncludeIf.HandlebarValue(true.toString)
  }

  case class SubmissionConsolidator(
    id: DestinationId,
    projectId: ProjectId,
    customerId: Expr,
    formData: Option[String],
    includeIf: DestinationIncludeIf,
    failOnError: Boolean
  ) extends Destination with DestinationWithCustomerId

  object SubmissionConsolidator {
    implicit val reads: Reads[SubmissionConsolidator] = Json.reads[SubmissionConsolidator]
  }

  case class Email(
    id: DestinationId,
    emailVerifierService: EmailVerifierService,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    to: FormComponentId,
    personalisation: Map[NotifierPersonalisationFieldId, FormComponentId]
  ) extends Destination

  case class PegaApi(
    id: DestinationId,
    includeIf: DestinationIncludeIf,
    failOnError: Boolean,
    caseId: Expr
  ) extends Destination with DestinationWithPegaCaseId

  val typeDiscriminatorFieldName: String = "type"
  val hmrcDms: String = "hmrcDms"
  val dataStore: String = "hmrcIlluminate"
  val infoArchive: String = "hmrcInfoArchive"
  val submissionConsolidator: String = "submissionConsolidator"
  val handlebarsHttpApi: String = "handlebarsHttpApi"
  val composite: String = "composite"
  val stateTransition: String = "stateTransition"
  val log: String = "log"
  val email: String = "email"
  val pegaApi: String = "pegaApi"

  private implicit def nonEmptyListOfDestinationsFormat: OFormat[NonEmptyList[Destination]] =
    derived.oformat[NonEmptyList[Destination]]()

  implicit def format: OFormat[Destination] = {
    implicit val personalisationReads =
      JsonUtils.formatMap[NotifierPersonalisationFieldId, FormComponentId](NotifierPersonalisationFieldId(_), _.value)

    OFormatWithTemplateReadFallback(
      ADTFormat.adtRead[Destination](
        typeDiscriminatorFieldName,
        hmrcDms                -> UploadableHmrcDmsDestination.reads,
        dataStore              -> UploadableDataStoreDestination.reads,
        infoArchive            -> UploadableInfoArchiveDestination.reads,
        submissionConsolidator -> UploadableSubmissionConsolidator.reads,
        handlebarsHttpApi      -> UploadableHandlebarsHttpApiDestination.reads,
        composite              -> UploadableCompositeDestination.reads,
        stateTransition        -> UploadableStateTransitionDestination.reads,
        log                    -> UploadableLogDestination.reads,
        email                  -> UploadableEmailDestination.reads,
        pegaApi                -> UploadablePegaApiDestination.reads
      )
    )
  }

  implicit val leafExprs: LeafExpr[Destination] = (path: TemplatePath, t: Destination) =>
    t match {
      case d: DestinationWithCustomerId => List(ExprWithPath(path + "customerId", d.customerId()))
      case d: DestinationWithPaymentReference =>
        List(ExprWithPath(path + "paymentReference", d.paymentReference)) ++
          List(
            d.nino.map(n => ExprWithPath(path + "nino", n)).toList,
            d.utr.map(u => ExprWithPath(path + "utr", u)).toList,
            d.postalCode.map(pc => ExprWithPath(path + "postalCode", pc)).toList
          ).flatten
      case d: DestinationWithPegaCaseId => List(ExprWithPath(path + "caseId", d.caseId))
      case _                            => Nil
    }
}

case class UploadableHmrcDmsDestination(
  id: DestinationId,
  dmsFormId: String,
  customerId: TextExpression,
  classificationType: String,
  businessArea: String,
  convertSingleQuotes: Option[Boolean],
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  dataOutputFormat: Option[DataOutputFormat],
  formdataXml: Option[Boolean],
  closedStatus: Option[Boolean],
  instructionPdfFields: Option[InstructionPdfFields] = None
) {

  def toHmrcDmsDestination: Either[String, Destination.HmrcDms] =
    for {
      cvii <- addErrorInfo(id, convertSingleQuotes, includeIf)
    } yield Destination.HmrcDms(
      id,
      dmsFormId,
      customerId.expr,
      classificationType,
      businessArea,
      cvii,
      failOnError.getOrElse(true),
      dataOutputFormat,
      formdataXml.getOrElse(false),
      closedStatus,
      instructionPdfFields,
      convertSingleQuotes,
      None,
      TemplateType.XML
    )
}

object UploadableHmrcDmsDestination {
  implicit val reads: Reads[Destination.HmrcDms] = new Reads[Destination.HmrcDms] {
    private val d: Reads[UploadableHmrcDmsDestination] = derived.reads[UploadableHmrcDmsDestination]()
    override def reads(json: JsValue): JsResult[Destination.HmrcDms] =
      d.reads(json).flatMap(_.toHmrcDmsDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableSubmissionConsolidator(
  id: DestinationId,
  projectId: ProjectId,
  customerId: TextExpression,
  formData: Option[String],
  convertSingleQuotes: Option[Boolean],
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean]
) {
  def toSubmissionConsolidatorDestination: Either[String, Destination.SubmissionConsolidator] =
    for {
      cvii     <- addErrorInfo(id, convertSingleQuotes, includeIf)
      formData <- addErrorInfo(id, "formData")(condition(convertSingleQuotes, formData))
    } yield SubmissionConsolidator(
      id,
      projectId,
      customerId.expr,
      formData,
      cvii,
      failOnError.getOrElse(true)
    )
}

case class UploadableDataStoreDestination(
  id: DestinationId,
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  formId: FormId,
  version: String,
  taxpayerId: TextExpression,
  regime: String,
  includeSessionInfo: Option[Boolean],
  convertSingleQuotes: Option[Boolean],
  handlebarPayload: Boolean,
  formDataPayload: Boolean,
  routing: SdesDestination,
  validateHandlebarPayload: Option[Boolean],
  jsonSchema: Option[JsValue]
) {
  private def toDataStoreDestination: Either[String, Destination.DataStore] =
    for {
      cvii <- addErrorInfo(id, convertSingleQuotes, includeIf)
    } yield Destination.DataStore(
      id,
      routing,
      cvii,
      failOnError.getOrElse(false),
      formId,
      version,
      taxpayerId.expr,
      regime,
      includeSessionInfo.getOrElse(false),
      handlebarPayload,
      formDataPayload,
      convertSingleQuotes,
      None,
      validateHandlebarPayload.getOrElse(false),
      jsonSchema
    )
}

object UploadableDataStoreDestination {
  implicit val reads: Reads[Destination.DataStore] = new Reads[Destination.DataStore] {
    implicit val formIdFormat: Format[FormId] = FormId.destformat
    private val d: Reads[UploadableDataStoreDestination] = derived.reads[UploadableDataStoreDestination]()
    override def reads(json: JsValue): JsResult[Destination.DataStore] =
      d.reads(json).flatMap(_.toDataStoreDestination.fold(JsError(_), JsSuccess(_)))
  }
}

object UploadableInfoArchiveDestination {
  implicit val reads: Reads[Destination.InfoArchive] = new Reads[Destination.InfoArchive] {
    implicit val formIdFormat: Format[FormId] = FormId.destformat
    private val d: Reads[UploadableInfoArchiveDestination] = derived.reads[UploadableInfoArchiveDestination]()
    override def reads(json: JsValue): JsResult[Destination.InfoArchive] =
      d.reads(json).flatMap(_.toInfoArchiveDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableInfoArchiveDestination(
  id: DestinationId,
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  formId: FormId,
  paymentReference: TextExpression,
  nino: Option[TextExpression],
  utr: Option[TextExpression],
  postalCode: Option[TextExpression]
) {
  private def toInfoArchiveDestination: Either[String, Destination.InfoArchive] =
    for {
      cvii <- addErrorInfo(id, None, includeIf)
    } yield Destination.InfoArchive(
      id,
      cvii,
      failOnError.getOrElse(false),
      formId,
      paymentReference.expr,
      nino.map(_.expr),
      utr.map(_.expr),
      postalCode.map(_.expr)
    )
}

object UploadableSubmissionConsolidator {
  implicit val reads: Reads[Destination.SubmissionConsolidator] = new Reads[Destination.SubmissionConsolidator] {
    private val d: Reads[UploadableSubmissionConsolidator] = derived.reads[UploadableSubmissionConsolidator]()
    override def reads(json: JsValue): JsResult[Destination.SubmissionConsolidator] =
      d.reads(json).flatMap(_.toSubmissionConsolidatorDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableCompositeDestination(
  id: DestinationId,
  convertSingleQuotes: Option[Boolean],
  includeIf: DestinationIncludeIf,
  destinations: NonEmptyList[Destination]
) {
  private def toCompositeDestination: Either[String, Destination.Composite] =
    for {
      cvii <- addErrorInfo(id, convertSingleQuotes, includeIf)
    } yield Destination.Composite(
      id,
      cvii,
      destinations
    )
}

object UploadableCompositeDestination {
  implicit val reads: Reads[Destination.Composite] = new Reads[Destination.Composite] {
    private val d: Reads[UploadableCompositeDestination] = derived.reads[UploadableCompositeDestination]()
    override def reads(json: JsValue): JsResult[Destination.Composite] =
      d.reads(json).flatMap(_.toCompositeDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableStateTransitionDestination(
  id: DestinationId,
  requiredState: String,
  convertSingleQuotes: Option[Boolean],
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean] = None
) {
  private def toStateTransitionDestination: Either[String, Destination.StateTransition] =
    for {
      cvii <- addErrorInfo(id, convertSingleQuotes, includeIf)
      rs <- addErrorInfo(id, "requiredState")(
              FormStatus
                .unapply(requiredState)
                .fold[Either[String, FormStatus]](
                  s"Invalid requiredState: '$requiredState'. Allowed states are: ${FormStatus.all}".asLeft
                )(_.asRight)
            )
    } yield Destination.StateTransition(
      id,
      rs,
      cvii,
      failOnError.getOrElse(true)
    )
}

object UploadableStateTransitionDestination {
  implicit val reads: Reads[Destination.StateTransition] = new Reads[Destination.StateTransition] {
    private val d: Reads[UploadableStateTransitionDestination] = derived.reads[UploadableStateTransitionDestination]()
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
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  multiRequestPayload: Option[Boolean]
) {
  def toHandlebarsHttpApiDestination: Either[String, Destination.HandlebarsHttpApi] =
    for {
      cvp   <- addErrorInfo(id, "payload")(conditionAndValidate(convertSingleQuotes, payload))
      cvii  <- addErrorInfo(id, convertSingleQuotes, includeIf)
      cvuri <- addErrorInfo(id, "uri")(condition(convertSingleQuotes, uri))
    } yield Destination
      .HandlebarsHttpApi(
        id,
        profile,
        cvuri,
        method,
        cvp,
        payloadType.getOrElse(TemplateType.JSON),
        cvii,
        failOnError.getOrElse(true),
        multiRequestPayload.getOrElse(false),
        convertSingleQuotes
      )
}

object UploadableHandlebarsHttpApiDestination {
  implicit val reads: Reads[Destination.HandlebarsHttpApi] = new Reads[Destination.HandlebarsHttpApi] {
    private val d = derived.reads[UploadableHandlebarsHttpApiDestination]()
    override def reads(json: JsValue): JsResult[Destination.HandlebarsHttpApi] =
      d.reads(json).flatMap(_.toHandlebarsHttpApiDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableLogDestination(id: DestinationId) {
  private def toLogDestination: Either[String, Destination.Log] = Right(Destination.Log(id))
}

object UploadableLogDestination {
  implicit val reads: Reads[Destination.Log] = new Reads[Destination.Log] {
    private val d: Reads[UploadableLogDestination] = derived.reads[UploadableLogDestination]()
    override def reads(json: JsValue): JsResult[Destination.Log] =
      d.reads(json).flatMap(_.toLogDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableEmailDestination(
  id: DestinationId,
  emailTemplateId: LocalisedEmailTemplateId,
  convertSingleQuotes: Option[Boolean],
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  to: FormComponentId,
  personalisation: Map[NotifierPersonalisationFieldId, FormComponentId]
) {
  private def toEmailDestination: Either[String, Destination.Email] =
    for {
      cvii <- addErrorInfo(id, convertSingleQuotes, includeIf)
    } yield Destination
      .Email(
        id,
        emailTemplateId.toNotify,
        cvii,
        failOnError.getOrElse(true),
        to,
        personalisation
      )
}

object UploadableEmailDestination {
  private implicit val personalisationReads: Format[Map[NotifierPersonalisationFieldId, FormComponentId]] =
    JsonUtils.formatMap[NotifierPersonalisationFieldId, FormComponentId](NotifierPersonalisationFieldId(_), _.value)

  implicit val reads: Reads[Destination.Email] = new Reads[Destination.Email] {
    private val d: Reads[UploadableEmailDestination] = derived.reads[UploadableEmailDestination]()
    override def reads(json: JsValue): JsResult[Destination.Email] =
      d.reads(json).flatMap(_.toEmailDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadablePegaApiDestination(
  id: DestinationId,
  includeIf: DestinationIncludeIf,
  failOnError: Option[Boolean],
  caseId: TextExpression
) {
  private def toPegaApiDestination: Either[String, Destination.PegaApi] =
    for {
      cvii <- addErrorInfo(id, None, includeIf)
    } yield Destination.PegaApi(
      id,
      cvii,
      failOnError.getOrElse(true),
      caseId.expr
    )
}

object UploadablePegaApiDestination {
  implicit val reads: Reads[Destination.PegaApi] = new Reads[Destination.PegaApi] {
    private val d: Reads[UploadablePegaApiDestination] = derived.reads[UploadablePegaApiDestination]()
    override def reads(json: JsValue): JsResult[Destination.PegaApi] =
      d.reads(json).flatMap(_.toPegaApiDestination.fold(JsError(_), JsSuccess(_)))
  }
}

object UploadableConditioning {

  def addErrorInfo(
    id: DestinationId,
    convertSingleQuotes: Option[Boolean],
    includeIf: DestinationIncludeIf
  ): Either[String, DestinationIncludeIf] =
    includeIf match {
      case HandlebarValue(s) => addErrorInfo(id, "includeIf")(condition(convertSingleQuotes, s)).map(HandlebarValue)
      case IncludeIfValue(i) => Right(IncludeIfValue(i))
    }

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
