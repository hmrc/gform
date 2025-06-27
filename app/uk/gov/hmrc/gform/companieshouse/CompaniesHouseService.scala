package uk.gov.hmrc.gform.companieshouse

import uk.gov.hmrc.circuitbreaker.{CircuitBreakerConfig, UsingCircuitBreaker}
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.http.{BadRequestException, NotFoundException}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import scala.concurrent.ExecutionContext

class CompaniesHouseService(connector: CompaniesHouseConnector, configuration: AppConfig)(implicit ec: ExecutionContext)
  extends UsingCircuitBreaker
    with BackendHeaderCarrierProvider {



  override def breakOnException(t: Throwable): Boolean = t match {
    case _: NotFoundException | _: BadRequestException => false
    case _                                             => true
  }

  override protected def circuitBreakerConfig: CircuitBreakerConfig = CircuitBreakerConfig(
    "companies-house",
    configuration.get[Int]("circuit.breaker.numberOfCallsToTrigger"),
    configuration.get[Int]("circuit.breaker.unavailablePeriodDurationInSec") * 1000,
    configuration.get[Int]("circuit.breaker.unstablePeriodDurationInSec") * 1000
  )
}
