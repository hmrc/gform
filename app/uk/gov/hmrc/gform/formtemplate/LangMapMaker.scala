/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.formtemplate
import cats.Applicative
import cats.instances.list._
import cats.syntax.traverse._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.LangADT

object LangMapMaker {

  implicit val ap: Applicative[JsResult] = new Applicative[JsResult] {

    override def pure[A](x: A): JsResult[A] = JsSuccess(x)
    override def ap[A, B](ff: JsResult[A => B])(fa: JsResult[A]): JsResult[B] =
      for {
        aToB <- ff
        a    <- fa
      } yield aToB(a)
  }

  private def convertToLangMap(jsValue: JsValue): JsResult[Map[LangADT, String]] =
    jsValue match {
      case JsString(message) => JsSuccess(Map(LangADT.En -> message))
      case jsObject: JsObject =>
        jsObject.value.toList
          .traverse[JsResult, (LangADT, String)] {
            case (k, JsString(message)) =>
              val langADT: JsResult[LangADT] =
                k match {
                  case "en" => JsSuccess(LangADT.En)
                  case "cy" => JsSuccess(LangADT.Cy)
                  case l    => JsError("Unsupported language " + l)
                }
              langADT.map(l => l -> message)
            case (_, other) => JsError("Expected string, got " + other)
          }
          .map(it => it.toMap)
      case other => JsError("Expected string, or language pair, got " + other)
    }

  val langADTMapReads: Reads[Map[LangADT, String]] = Reads.apply[Map[LangADT, String]](convertToLangMap)
}
