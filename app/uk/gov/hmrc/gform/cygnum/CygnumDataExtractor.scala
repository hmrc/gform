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

package uk.gov.hmrc.gform.cygnum

import cats.{ Applicative, Monad }
import cats.implicits._
import cats.syntax.applicative._

import scala.xml.Elem

trait CygnumDataExtractorAlg[F[_]] {
  def getDataResult(elm: Elem): F[String]
  def urn(elm: String): F[String]
  def unEscapeXml(xmlAsString: String): F[Elem]
}

class CygnumDataExtractor[F[_]: Monad](extractorAlg: CygnumDataExtractorAlg[F]) {

  def extractUrn(payload: String): F[String] =
    for {
      xmlResponse <- extractorAlg.unEscapeXml(payload)
      escapedXml  <- extractorAlg.getDataResult(xmlResponse)
      urn         <- extractorAlg.urn(escapedXml)
    } yield urn
}

class CygnumDataExtractorProgram[F[_]](implicit M: Applicative[F]) extends CygnumDataExtractorAlg[F] {
  override def getDataResult(elm: Elem): F[String] =
    M.pure((elm \\ "GetDataResult").text)

  override def urn(elm: String): F[String] =
    unEscapeXml(elm).map(e => (e \\ "URN").text)

  override def unEscapeXml(xmlAsString: String): F[Elem] =
    M.pure(scala.xml.XML.loadString(xmlAsString))
}
