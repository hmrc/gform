package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.core.parsers.ExprParsers
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

case class Validators(validatorName: String, errorMessage: String, parameters: Map[String, Seq[FormCtx]]) {

  def getValidator = {
    validatorName match {
      case "hmrcUTRPostcodeCheck" => HMRCUTRPostcodeCheck(parameters("utr").head, parameters("postCode").head, errorMessage)
      case _ => Somethingelse(validatorName)
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

  def validate(f: A => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]]
}

case class HMRCUTRPostcodeCheck(utr: FormCtx, postcode: FormCtx, errorMessage: String) extends Validator[Seq[FormCtx]] {

  override def validate(f: Seq[FormCtx] => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]] =
    f(Seq(utr, postcode)).map(if (_) Valid(()) else Invalid(Map(FieldId("B") -> Set(errorMessage))))

}

case class Somethingelse(value: String) extends Validator[String] {
  override def validate(f: (String) => Future[Boolean])(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Validated[Map[FieldId, Set[String]], Unit]] = ???
}
