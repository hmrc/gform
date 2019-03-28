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

sealed trait Destination extends Product with Serializable {
  def id: DestinationId
  def includeIf: Option[String]
  def failOnError: Option[Boolean]

  def failOnErrorDefaulted: Boolean = failOnError.getOrElse(true)
}

object Destination {
  case class HmrcDms(
    id: DestinationId,
    dmsFormId: String,
    customerId: TextExpression,
    classificationType: String,
    businessArea: String,
    includeIf: Option[String] = None,
    failOnError: Option[Boolean] = None,
    roboticsXml: Option[Boolean] = None)
      extends Destination {
    def toDeprecatedDmsSubmission: Destinations.DmsSubmission = Destinations.DmsSubmission(
      dmsFormId,
      customerId,
      classificationType,
      businessArea,
      roboticsXml = roboticsXml
    )
  }

  case class HandlebarsHttpApi(
    id: DestinationId,
    profile: Profile,
    uri: String,
    method: HttpMethod,
    payload: Option[String],
    includeIf: Option[String] = None,
    failOnError: Option[Boolean] = None)
      extends Destination

  val typeDiscriminatorFieldName: String = "type"
  val hmrcDms: String = "hmrcDms"
  val handlebarsHttpApi: String = "handlebarsHttpApi"

  implicit val format: OFormat[Destination] = {
    implicit val d: OFormat[Destination] = derived.oformat

    OFormatWithTemplateReadFallback(
      ADTFormat.adtRead[Destination](
        typeDiscriminatorFieldName,
        hmrcDms           -> UploadableHmrcDmsDestination.reads,
        handlebarsHttpApi -> UploadableHandlebarsHttpApiDestination.reads
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
    } yield Destination.HmrcDms(id, dmsFormId, customerId, classificationType, businessArea, cii, failOnError)
}

object UploadableHmrcDmsDestination {
  implicit val reads: Reads[Destination.HmrcDms] = new Reads[Destination.HmrcDms] {
    private val d: Reads[UploadableHmrcDmsDestination] = derived.reads[UploadableHmrcDmsDestination]
    override def reads(json: JsValue): JsResult[Destination.HmrcDms] =
      d.reads(json).flatMap(_.toHmrcDmsDestination.fold(JsError(_), JsSuccess(_)))
  }
}

case class UploadableHandlebarsHttpApiDestination(
  id: DestinationId,
  profile: Profile,
  uri: String,
  method: HttpMethod,
  payload: Option[String],
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
        .HandlebarsHttpApi(id, profile, cvuri, method, cvp, cvii, failOnError)
}

object UploadableHandlebarsHttpApiDestination {
  implicit val reads: Reads[Destination.HandlebarsHttpApi] = new Reads[Destination.HandlebarsHttpApi] {
    private val d = derived.reads[UploadableHandlebarsHttpApiDestination]
    override def reads(json: JsValue): JsResult[Destination.HandlebarsHttpApi] =
      d.reads(json).flatMap(_.toHandlebarsHttpApiDestination.fold(JsError(_), JsSuccess(_)))
  }
}

object UploadableConditioning {
  def addErrorInfo[T](destinationId: DestinationId, field: String)(result: Either[String, T]) =
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
