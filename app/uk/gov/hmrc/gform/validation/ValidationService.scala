package uk.gov.hmrc.gform.validation

import uk.gov.hmrc.gform.bank_account_reputation.BankAccountReputationConnector
import uk.gov.hmrc.gform.des.{AddressDes, DesConnector}
import uk.gov.hmrc.play.http.{HeaderCarrier, NotFoundException}

import scala.concurrent.Future


class ValidationService(desConnector: DesConnector, bankAccountReputationConnector: BankAccountReputationConnector) {


  def callDes(utr: String, postCode: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    def compare(address: AddressDes) = {
      address.postalCode.replace(" ", "").equalsIgnoreCase(postCode.replace(" ", "")) || address.postalCode == "Valid"
    }
    desConnector.lookup(utr).map(compare)
      .recover {
        case _: NotFoundException => false
      }
  }

  def callBRS(accountNumber: String, sortCode: String): Future[Boolean] =
    bankAccountReputationConnector.exists(accountNumber, sortCode).map(_.accountNumberWithSortCodeIsValid)


}
