/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.ExprParsers
import uk.gov.hmrc.play.http.HeaderCarrier
import play.api.libs.functional.syntax._

import scala.concurrent.{ ExecutionContext, Future }

case class Validators(validatorName: String, errorMessage: String, parameters: Map[String, FormCtx]) {

  def getValidator = {
    validatorName match {
      case "hmrcUTRPostcodeCheck" => HMRCUTRPostcodeCheck(parameters("utr"), parameters("postCode"), errorMessage)
    }
  }
}

object Validators {

  lazy val writes = Json.writes[Validators]
  lazy val readsValidator = Json.reads[Validators]
  implicit val reads: Reads[FormCtx] = readsForMongoJson | readsForTemplateJson
  private lazy val readsForMongoJson = Json.reads[FormCtx]

  private lazy val readsForTemplateJson = Reads { json =>
    exprParser(json)
  }

  lazy implicit val format: OFormat[Validators] = OFormat(readsValidator, writes)

  private def exprParser(json: JsValue): JsResult[FormCtx] = {
    json match {
      case JsString(exprAsStr) => parse(exprAsStr)
      case otherwise => JsError(s"Invalid expression. Expected String, got $otherwise")
    }
  }

  private def parse(exprAsStr: String): JsResult[FormCtx] =
    ExprParsers.validate(exprAsStr) fold (
      error => JsError(error.toString),
      expr => JsSuccess(expr)
    )
}

trait Validator[A] {

  def validate(data: Map[FieldId, Seq[String]])(f: A => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]]
}

case class HMRCUTRPostcodeCheck(utr: FormCtx, postcode: FormCtx, errorMessage: String) extends Validator[(String, String)] {

  val utrFieldId = FieldId(utr.value)
  val postcodeFieldId = FieldId(postcode.value)

  override def validate(data: Map[FieldId, Seq[String]])(f: ((String, String)) => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]] = {
    val dataGetter: FieldId => String = id => data.get(id).toList.flatten.headOption.getOrElse("")
    val utrString = dataGetter(utrFieldId)
    val postCodeString = dataGetter(postcodeFieldId)
    f(utrString -> postCodeString).map(if (_) Valid(()) else Invalid(Map(utrFieldId -> Set(errorMessage), postcodeFieldId -> Set(errorMessage))))
  }
}
