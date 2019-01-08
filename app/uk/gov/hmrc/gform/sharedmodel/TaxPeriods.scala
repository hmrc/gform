package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{Json}

case class TaxPeriods(taxPeriods: List[TaxPeriod])

object TaxPeriods{
  implicit val format = Json.format[TaxPeriods]
}

case class TaxPeriod(inboundCorrespondenceFromDate: String, inboundCorrespondenceToDate: String, periodKey: String)

object TaxPeriod {
  implicit val format = Json.format[TaxPeriod]
}
