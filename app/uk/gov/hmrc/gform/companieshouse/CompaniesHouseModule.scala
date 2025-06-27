package uk.gov.hmrc.gform.companieshouse

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

import scala.concurrent.ExecutionContext

class CompaniesHouseModule(
                            configModule: ConfigModule,
                            wSHttpModule: WSHttpModule
                          )(implicit ex: ExecutionContext) {

  val companiesHouseConnector = new CompaniesHouseConnector(wSHttpModule.auditableWSHttp, configModule.companiesHouseConfig)
  val companiesHouseService = new

}
