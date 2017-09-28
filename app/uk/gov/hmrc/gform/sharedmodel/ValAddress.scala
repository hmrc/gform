package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.Json

case class ValAddress(utr: String, postCode: String)

object ValAddress {
  implicit val format = Json.format[ValAddress]
}
