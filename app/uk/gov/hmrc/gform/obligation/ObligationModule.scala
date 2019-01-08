package uk.gov.hmrc.gform.obligation

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.des.DesConnector
import uk.gov.hmrc.gform.wshttp.WSHttpModule


class ObligationModule(wSHttpModule: WSHttpModule, configModule: ConfigModule) {

  private val desConfig = configModule.desConfig
  private val desConnector: DesConnector =
    new DesConnector(wSHttpModule.auditableWSHttp, configModule.serviceConfig.baseUrl("etmp-hod"), desConfig)


  private val obligationService = new ObligationService(desConnector)
  val obligationController = new ObligationController(obligationService)
}
