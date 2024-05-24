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

package uk.gov.hmrc.gform.sharedmodel

import cats.implicits._
import play.api.libs.json.{ JsError, JsString, JsSuccess, Reads }
import java.net.URLEncoder
import play.api.mvc.{ JavascriptLiteral, PathBindable, QueryStringBindable }
import scala.util.Try
import uk.gov.hmrc.crypto.Crypted
import uk.gov.hmrc.gform.builder.SectionPath
import uk.gov.hmrc.gform.history.HistoryId
import uk.gov.hmrc.gform.sharedmodel.dblookup.{ CollectionName, DbLookupId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Coordinates, FormTemplateId, FormTemplateRawId, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.sharedmodel.sdes._
import uk.gov.hmrc.mongo.workitem.ProcessingStatus
import uk.gov.hmrc.mongo.workitem.ProcessingStatus.Implicits.format

object ValueClassBinder {

  implicit val jLiteralAffinityGroup: JavascriptLiteral[AffinityGroup] = new JavascriptLiteral[AffinityGroup] {
    def to(value: AffinityGroup): String = AffinityGroupUtil.affinityGroupName(value)
  }
  implicit val notifierEmailAddressBinder: PathBindable[NotifierEmailAddress] = valueClassBinder(_.value)
  implicit val affinityGroupBinder: PathBindable[Option[AffinityGroup]] = mValueClassBinder(
    AffinityGroupUtil.affinityGroupName
  )
  implicit val formTemplateIdBinder: PathBindable[FormTemplateId] = caseInsensitive(FormTemplateId.apply, _.value)
  implicit val formTemplateRawIdBinder: PathBindable[FormTemplateRawId] =
    caseInsensitive(FormTemplateRawId.apply, _.value)

  implicit val historyIdBinder: PathBindable[HistoryId] = {
    implicit val read: Reads[HistoryId] = HistoryId.flatReads
    valueClassBinder(_.id)
  }
  implicit val accessCodeBinder: PathBindable[AccessCode] = valueClassBinder(_.value)
  implicit val destinationIdBinder: PathBindable[DestinationId] = valueClassBinder(_.id)
  implicit val formIdBinder: PathBindable[FormId] = valueClassBinder(_.value)
  implicit val envelopeIdBinder: PathBindable[EnvelopeId] = valueClassBinder(_.value)
  implicit val userIdBinder: PathBindable[UserId] = valueClassBinder(_.value)
  implicit val fileIdBinder: PathBindable[FileId] = valueClassBinder(_.value)
  implicit val dbLookupIdBinder: PathBindable[DbLookupId] = valueClassBinder(_.id)
  implicit val collectionNameBinder: PathBindable[CollectionName] = valueClassBinder(_.name)
  implicit val handlebarsPayloadNameBinder: PathBindable[HandlebarsTemplateId] = valueClassBinder(_.value)
  implicit val notificationStatusBinder: QueryStringBindable[NotificationStatus] = valueClassQueryBinder(
    NotificationStatus.fromName
  )
  implicit val sdesDestinationBinder: QueryStringBindable[SdesDestination] = valueClassQueryBinder(
    SdesDestination.fromName
  )

  implicit object sectionPathBindable
      extends QueryStringBindable.Parsing[SectionPath](
        SectionPath.apply,
        sectionPath => URLEncoder.encode(sectionPath.value, "utf-8"),
        (key: String, e: Exception) => "Cannot parse parameter %s as SectionPath: %s".format(key, e.getMessage)
      )

  implicit val envelopeIdQueryBinder: QueryStringBindable[EnvelopeId] = valueClassQueryBinder(_.value)
  implicit val processingStatusBinder: QueryStringBindable[ProcessingStatus] = valueClassQueryBinder(_.name)
  implicit val bannerIdBinder: PathBindable[BannerId] = valueClassBinder(_.value)
  implicit val shutterMessageIdBinder: PathBindable[ShutterMessageId] = valueClassBinder(_.value)

  implicit val reads: Reads[Crypted] = Reads {
    case JsString(str) => JsSuccess(Crypted(str))
    case unknown       => JsError(s"Failed to read Crypted. Expected JsString, but found $unknown")
  }
  implicit val cryptedBinder: QueryStringBindable[Crypted] = valueClassQueryBinder(_.value)

  implicit val sectionNumberBinder: PathBindable[SectionNumber] = new PathBindable[SectionNumber] {
    override def bind(key: String, value: String): Either[String, SectionNumber] =
      Try(SectionNumber(value.toInt)).map(_.asRight).getOrElse(s"No valid value in path $key: $value".asLeft)
    override def unbind(key: String, sectionNumber: SectionNumber): String = sectionNumber.value.toString
  }

  implicit val formStatusBinder: PathBindable[FormStatus] = new PathBindable[FormStatus] {
    override def bind(key: String, value: String): Either[String, FormStatus] =
      value match {
        case FormStatus(s) => s.asRight
        case _             => s"'$value' is not a valid FormStatus. Valid values are: ${FormStatus.all}".asLeft
      }

    override def unbind(key: String, value: FormStatus): String = value.toString
  }

  implicit val coordinatesQueryBinder: QueryStringBindable[Coordinates] = new QueryStringBindable[Coordinates] {

    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, Coordinates]] =
      params.get(key).flatMap(_.headOption).map { value =>
        Coordinates
          .parse(value)
          .map(_.asRight)
          .getOrElse(s"No valid value in path $key: $value".asLeft)
      }

    override def unbind(key: String, coordinates: Coordinates): String =
      s"""$key=${coordinates.value.toString}"""
  }

  def valueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) = {

    def parseString(str: String) =
      JsString(str).validate[A] match {
        case JsSuccess(a, _) => Right(a)
        case JsError(error)  => Left(s"No valid value in path: $str. Error: $error")
      }

    new PathBindable[A] {
      override def bind(key: String, value: String): Either[String, A] =
        stringBinder.bind(key, value).flatMap(parseString)

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }
  }

  def mValueClassBinder[A: Reads](fromAtoString: A => String)(implicit stringBinder: PathBindable[String]) = {

    def parseString(str: String) =
      if (str.isEmpty) Right(None)
      else
        JsString(str).validate[A] match {
          case JsSuccess(a, _) => Right(Some(a))
          case JsError(error)  => Left(s"No valid value in path: $str. Error: $error")
        }

    new PathBindable[Option[A]] {
      override def bind(key: String, value: String): Either[String, Option[A]] =
        stringBinder.bind(key, value).flatMap(parseString)

      override def unbind(key: String, ma: Option[A]): String =
        ma.fold("")(a => stringBinder.unbind(key, fromAtoString(a)))
    }
  }

  def caseInsensitive[A: Reads](f: String => A, g: A => String) =
    implicitly[PathBindable[String]].transform[A](s => f(s.toLowerCase), a => g(a).toLowerCase)

  private def valueClassQueryBinder[A: Reads](
    fromAtoString: A => String
  )(implicit stringBinder: QueryStringBindable[String]) =
    new QueryStringBindable[A] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, A]] =
        stringBinder.bind(key, params).map(_.flatMap(parseString[A]))

      override def unbind(key: String, a: A): String =
        stringBinder.unbind(key, fromAtoString(a))
    }

  private def parseString[A: Reads](str: String) =
    JsString(str).validate[A] match {
      case JsSuccess(a, _) => Right(a)
      case JsError(error)  => Left("No valid value in url binding: " + str + ". Error: " + error)
    }
}
