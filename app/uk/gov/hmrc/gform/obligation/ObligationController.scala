package uk.gov.hmrc.gform.obligation

import play.api.Logger
import play.api.mvc.Action
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.controllers.BaseController
import uk.gov.hmrc.gform.des.TaxPeriodDes
import uk.gov.hmrc.gform.sharedmodel.{TaxPeriods, TaxPeriod}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.HmrcTaxPeriod
import scala.concurrent.duration._


class ObligationController(obligation: ObligationService)extends BaseController{

  def getTaxPeriods(idType:String, idNumber:String,regimeType:String) = Action.async(parse.json[HmrcTaxPeriod]) { implicit request =>
    Logger.info(s"Get Tax Periods from DES, ${loggingHelpers.cleanHeaders(request.headers)}")
    obligation.callDES(request.body).map(i => Ok(new TaxPeriods(i.obligationDetails.map(
      j => new TaxPeriod(j.inboundCorrespondenceFromDate, j.inboundCorrespondenceToDate, j.periodKey)
    ))))
  }
}