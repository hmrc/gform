/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.integrationFramework

import julienrf.json.derived
import play.api.libs.json.OFormat

case class GeneralDecisionDetails(emailConsent: Boolean)

object GeneralDecisionDetails {
  implicit val format: OFormat[GeneralDecisionDetails] = derived.oformat()
}

case class PodUkAddress(
  name: Option[String],
  streetAndNumber: String,
  postCode: Option[String],
  city: String,
  countryCode: Option[String]
)

object PodUkAddress {
  implicit val format: OFormat[PodUkAddress] = derived.oformat()
}

case class PodExporterDetails(
  eori: String,
  fullName: String,
  ukAddress: PodUkAddress
)

object PodExporterDetails {
  implicit val format: OFormat[PodExporterDetails] = derived.oformat()
}

case class PodYourDetails(
  fullName: String,
  addressLine1: String,
  addressLine2: String,
  postCode: String,
  city: String,
  country: String,
  telephoneNumber: Option[String],
  emailAddress: String
)

object PodYourDetails {
  implicit val format: OFormat[PodYourDetails] = derived.oformat()
}

case class PodGoodsItem(hs4Code: String, goodsDescription: String)

object PodGoodsItem {
  implicit val format: OFormat[PodGoodsItem] = derived.oformat()
}

case class PodFtaDetails(
  exporterActivity: String,
  version: Int,
  lastPodAuthModifiedDate: String,
  goodsItems: List[PodGoodsItem]
)

object PodFtaDetails {
  implicit val format: OFormat[PodFtaDetails] = derived.oformat()
}

case class PodAuthorisationDetailsResponse(
  generalDecisionDetails: GeneralDecisionDetails,
  exporterDetails: PodExporterDetails,
  yourDetails: PodYourDetails,
  podFtaDetails: PodFtaDetails
) {

  // One row per goods item, with the non-goods fields repeated on every row. Ordering of goods items is
  // preserved as returned by PDS (ascending hs4Code, then goodsDescription).
  def transform: List[FlattenedPodAuthorisationDetails] = {
    def row(goodsItem: Option[PodGoodsItem]) =
      FlattenedPodAuthorisationDetails(
        emailConsent = generalDecisionDetails.emailConsent,
        exporterEori = exporterDetails.eori,
        exporterFullName = exporterDetails.fullName,
        exporterAddressName = exporterDetails.ukAddress.name,
        exporterStreetAndNumber = exporterDetails.ukAddress.streetAndNumber,
        exporterPostCode = exporterDetails.ukAddress.postCode,
        exporterCity = exporterDetails.ukAddress.city,
        exporterCountryCode = exporterDetails.ukAddress.countryCode,
        yourDetailsFullName = yourDetails.fullName,
        yourDetailsAddressLine1 = yourDetails.addressLine1,
        yourDetailsAddressLine2 = yourDetails.addressLine2,
        yourDetailsPostCode = yourDetails.postCode,
        yourDetailsCity = yourDetails.city,
        yourDetailsCountry = yourDetails.country,
        yourDetailsTelephoneNumber = yourDetails.telephoneNumber,
        yourDetailsEmailAddress = yourDetails.emailAddress,
        exporterActivity = podFtaDetails.exporterActivity,
        version = podFtaDetails.version,
        lastPodAuthModifiedDate = podFtaDetails.lastPodAuthModifiedDate,
        hs4Code = goodsItem.map(_.hs4Code),
        goodsDescription = goodsItem.map(_.goodsDescription)
      )

    podFtaDetails.goodsItems match {
      case Nil   => List(row(None))
      case items => items.map(item => row(Some(item)))
    }
  }
}

object PodAuthorisationDetailsResponse {
  implicit val format: OFormat[PodAuthorisationDetailsResponse] = derived.oformat()
}

case class FlattenedPodAuthorisationDetails(
  emailConsent: Boolean,
  exporterEori: String,
  exporterFullName: String,
  exporterAddressName: Option[String],
  exporterStreetAndNumber: String,
  exporterPostCode: Option[String],
  exporterCity: String,
  exporterCountryCode: Option[String],
  yourDetailsFullName: String,
  yourDetailsAddressLine1: String,
  yourDetailsAddressLine2: String,
  yourDetailsPostCode: String,
  yourDetailsCity: String,
  yourDetailsCountry: String,
  yourDetailsTelephoneNumber: Option[String],
  yourDetailsEmailAddress: String,
  exporterActivity: String,
  version: Int,
  lastPodAuthModifiedDate: String,
  hs4Code: Option[String],
  goodsDescription: Option[String]
)

object FlattenedPodAuthorisationDetails {
  implicit val format: OFormat[FlattenedPodAuthorisationDetails] = derived.oformat()
}
