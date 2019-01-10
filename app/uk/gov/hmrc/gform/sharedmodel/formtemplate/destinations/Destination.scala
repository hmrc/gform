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
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.parsing.combinator.RegexParsers

sealed trait Destination extends Product with Serializable {
  def id: DestinationId
}

object Destination {
  case class HmrcDms(
    id: DestinationId,
    dmsFormId: String,
    customerId: TextExpression,
    classificationType: String,
    businessArea: String)
      extends Destination {
    def toDeprecatedDmsSubmission: Destinations.DmsSubmission = Destinations.DmsSubmission(
      dmsFormId,
      customerId,
      classificationType,
      businessArea
    )
  }

  case class HandlebarsHttpApi(
    id: DestinationId,
    profile: Profile,
    uri: String,
    method: HttpMethod,
    payload: Option[String])
      extends Destination

  val typeDiscriminatorFieldName: String = "type"
  val hmrcDms: String = "hmrcDms"
  val handlebarsHttpApi: String = "handlebarsHttpApi"

  implicit val format: OFormat[Destination] = {
    implicit val d: OFormat[Destination] = derived.oformat

    OFormatWithTemplateReadFallback(
      ADTFormat.adtRead[Destination](
        typeDiscriminatorFieldName,
        hmrcDms           -> derived.reads[HmrcDms],
        handlebarsHttpApi -> UploadableHandlebarsHttpApiDestination.reads
      ))
  }
}

case class UploadableHandlebarsHttpApiDestination(
  id: DestinationId,
  profile: Profile,
  uri: String,
  method: HttpMethod,
  payload: Option[String],
  convertSingleQuotes: Option[Boolean]) {

  def toHandlebarsHttpApiDestination: Either[String, Destination.HandlebarsHttpApi] =
    conditionedPayload.map(Destination.HandlebarsHttpApi(id, profile, uri, method, _))

  private def conditionedPayload: Either[String, Option[String]] = payload match {
    case None => Right(None)
    case Some(p) =>
      if (convertSingleQuotes.getOrElse(false)) SingleQuoteReplacementLexer(p).map(Option(_))
      else Right(Option(p))
  }
}

object UploadableHandlebarsHttpApiDestination {
  implicit val reads = new Reads[Destination.HandlebarsHttpApi] {
    val d = derived.reads[UploadableHandlebarsHttpApiDestination]
    override def reads(json: JsValue): JsResult[Destination.HandlebarsHttpApi] =
      d.reads(json).flatMap(_.toHandlebarsHttpApiDestination.fold(JsError(_), JsSuccess(_)))
  }
}

object SingleQuoteReplacementLexer extends RegexParsers {
  override def skipWhitespace = false

  private def stringLiteral: Parser[String] = {
    val digit = s"\\d"
    val hexDigit = s"($digit|[A-Fa-f])"

    val normalCharacter = raw"""[^\\'"]""".r
    val unescapedDoubleQuote = "\"".r ^^ { _ =>
      "\\\""
    }
    val escapedSingleQuote = raw"""\\'""".r ^^ { _ =>
      "'"
    }
    val escapedNonUnicodeCharacter = raw"""\\[\\/bfnrt]""".r
    val escapedUnicodeCharacter = raw"""\\u$hexDigit{4}""".r
    val escapedCharacter: Parser[String] = escapedSingleQuote | escapedNonUnicodeCharacter | escapedUnicodeCharacter
    val character: Parser[String] = normalCharacter | escapedCharacter | unescapedDoubleQuote

    "'" ~> rep(character) <~ "'" ^^ { s =>
      "\"" + s.mkString("") + "\""
    }
  }

  def nonStringLiteral: Parser[String] = raw"""[^']+""".r

  private def tokens: Parser[List[String]] =
    phrase(rep1(stringLiteral | nonStringLiteral))

  def apply(code: String): Either[String, String] =
    parse(tokens, code) match {
      case NoSuccess(msg, _)  => Left(msg)
      case Success(result, _) => Right(result.mkString(""))
    }
}
