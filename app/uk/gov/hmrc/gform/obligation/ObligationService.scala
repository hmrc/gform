package uk.gov.hmrc.gform.obligation

import uk.gov.hmrc.gform.des.{DesConnector, TaxPeriodDes}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.sharedmodel.TaxPeriod
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod

import scala.concurrent.ExecutionContext
class ObligationService(desConnector: DesConnector) {

  def callDES(hmrcTaxPeriod: HmrcTaxPeriod)(implicit hc: HeaderCarrier, ex: ExecutionContext) = {
    desConnector.lookupTaxPeriod(hmrcTaxPeriod.idType, hmrcTaxPeriod.idNumber, hmrcTaxPeriod.regimeType)
  }

}
