package uk.gov.hmrc.gform.bank_account_reputation

import play.api.libs.json._
import uk.gov.hmrc.gform.config.DesConnectorConfig
import uk.gov.hmrc.gform.wshttp.WSHttp
import play.api.libs.functional.syntax._
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class BankAccountReputationConnector(wSHttp: WSHttp, baseUrl: String, basePath: String)  {

  def exists(accountNumber: String, sortCode: String)(implicit hc: HeaderCarrier): Future[Boolean] =
    wSHttp.POST[Account, Response](s"$baseUrl$basePath/modcheck", Account(sortCode, accountNumber)).map(_.accountNumberWithSortCodeIsValid)

}

case class Account(
                    sortCode: String,
                    accountNumber: String
                  )

object Account {
  implicit val format = Json.format[Account]
}

case class Response(
                     accountNumberWithSortCodeIsValid: Boolean,
                     nonStandardAccountDetailsRequiredForBacs: String
                   )

object Response {
  val reads = Reads[Response] { json =>
    (json \ "parameters" \ "nonStandardAccountDetailsRequiredForBacs").asOpt[String] match {
      case Some(str) => parse(json, str)
      case None => JsError("the response does not match desired parameters : accountNumberWithSortCodeIsValid, accountNumberWithSortCodeIsValid")
    }
  }

  def parse(json: JsValue, str: String) =
    str match {
      case "no" | "yes" | "inapplicable" => JsSuccess(Response((json \ "accountNumberWithSortCodeIsValid").as[Boolean], str))
      case _ => JsError("Response did not match no, yes, inapplicable")
    }

  val basic: OFormat[Response] = Json.format[Response]

  val readsAll = (basic : Reads[Response]) | reads
  val writes: OWrites[Response] = basic

  implicit val format = OFormat(readsAll, writes)
}