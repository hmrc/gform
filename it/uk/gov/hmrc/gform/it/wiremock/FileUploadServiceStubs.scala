package uk.gov.hmrc.gform.it.wiremock

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{ aResponse, stubFor, urlEqualTo }
import play.api.http.HeaderNames.LOCATION

trait FileUploadServiceStubs {
  def createEnvelopeStub() {
    stubFor(
      WireMock
        .post(urlEqualTo("/file-upload/envelopes"))
        .willReturn(
          aResponse()
            .withStatus(201)
            .withHeader(LOCATION, "envelopes/some-envelope-id")
        )
    )
    ()
  }
}
