package uk.gov.hmrc.gform.testonly

import uk.gov.hmrc.gform.wshttp.WSHttp

class EnrolmentConnector(wSHttp: WSHttp, baseUrl: String) {

  def deEnrol(userId: String, registrationNumber: String) =
    wSHttp.DELETE(s"$baseUrl/enrolment-store/users/$userId/enrolments/HMRC-OBTDS-ORG~EtmpRegistrationNumber~$registrationNumber")

}
