/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json._
import uk.gov.hmrc.gform.addresslookup.AddressLookupResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, DataRetrieveId, DataRetrieveResult, NotChecked, Obligations }
import uk.gov.hmrc.auth.core.retrieve.{ ItmpAddress, ItmpName }
import java.time.LocalDate

final case class ItmpRetrievals(
  itmpName: Option[ItmpName],
  itmpDateOfBirth: Option[LocalDate],
  itmpAddress: Option[ItmpAddress]
)
object ItmpRetrievals {
  implicit val itmpNameFormat: OFormat[ItmpName] = Json.format[ItmpName]
  implicit val itmpAddressFormat: OFormat[ItmpAddress] = Json.format[ItmpAddress]
  implicit val format: OFormat[ItmpRetrievals] = Json.format[ItmpRetrievals]
}

case class ThirdPartyData(
  obligations: Obligations,
  emailVerification: Map[FormComponentId, EmailAndCode],
  queryParams: QueryParams,
  reviewData: Option[Map[String, String]] = None,
  booleanExprCache: BooleanExprCache,
  dataRetrieve: Option[Map[DataRetrieveId, DataRetrieveResult]],
  postcodeLookup: Option[Map[FormComponentId, AddressLookupResult]],
  selectedAddresses: Option[Map[FormComponentId, String]],
  enteredAddresses: Option[Map[FormComponentId, FormData]],
  confirmedAddresses: Option[Set[FormComponentId]],
  itmpRetrievals: Option[ItmpRetrievals],
  confirmations: Option[Map[FormComponentId, List[String]]]
) {

  def reviewComments: Option[String] = reviewData.flatMap(_.get("caseworkerComment"))
}

object ThirdPartyData {
  val empty =
    ThirdPartyData(
      NotChecked,
      Map.empty,
      QueryParams.empty,
      None,
      BooleanExprCache.empty,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

  implicit val formatMap: Format[Map[FormComponentId, EmailAndCode]] =
    JsonUtils.formatMap(FormComponentId.apply, _.value)
  implicit val formatDataRetrieve: Format[Map[DataRetrieveId, DataRetrieveResult]] =
    JsonUtils.formatMap(DataRetrieveId(_), _.value)
  implicit val formatPostcodeLookup: Format[Map[FormComponentId, AddressLookupResult]] =
    JsonUtils.formatMap(FormComponentId(_), _.value)
  implicit val formatSelectedAddresses: Format[Map[FormComponentId, String]] =
    JsonUtils.formatMap(FormComponentId(_), _.value)
  implicit val formatEnteredAddresses: Format[Map[FormComponentId, FormData]] =
    JsonUtils.formatMap(FormComponentId(_), _.value)
  implicit val formatFormComponentId: Format[FormComponentId] =
    implicitly[Format[String]].bimap(FormComponentId(_), _.value)
  implicit val formatConfirmations: Format[Map[FormComponentId, List[String]]] =
    JsonUtils.formatMap(FormComponentId(_), _.value)

  implicit val format: OFormat[ThirdPartyData] = Json.format
}
