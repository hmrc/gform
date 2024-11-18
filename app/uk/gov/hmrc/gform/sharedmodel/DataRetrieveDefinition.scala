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

package uk.gov.hmrc.gform.sharedmodel

import cats.implicits._
import play.api.libs.json.{ JsObject, JsValue }
import uk.gov.hmrc.gform.core.Opt
import uk.gov.hmrc.gform.core.parsers.ValueParser
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.AttrType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IncludeIf

final case class DataRetrieveDefinitions(
  definitions: List[DataRetrieveDefinition]
)

final case class DataRetrieveDefinition(
  tpe: DataRetrieve.Type,
  attributes: Attr,
  parameters: List[DataRetrieve.Parameter],
  attrTypeMapping: Map[DataRetrieve.Attribute, DataRetrieve.AttrType] = Map.empty
)

object DataRetrieveDefinitions {
  import DataRetrieve.{ ParamExpr, Parameter, Type }
  val validateBankDetails = DataRetrieveDefinition(
    Type("validateBankDetails"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("isValid"),
          ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeBankName"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
          ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
      )
    ),
    List(
      Parameter("sortCode", List("account")),
      Parameter("accountNumber", List("account"))
    )
  )

  val businessBankAccountExistence = DataRetrieveDefinition(
    DataRetrieve.Type("businessBankAccountExistence"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("accountNumberIsWellFormatted"),
          ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeBankName"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
          ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountExists"),
          ConstructAttribute.AsIs(Fetch(List("accountExists")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nameMatches"),
          ConstructAttribute.AsIs(Fetch(List("nameMatches")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountName"),
          ConstructAttribute.AsIs(Fetch(List("accountName")))
        )
      )
    ),
    List(
      Parameter("sortCode", List("account")),
      Parameter("accountNumber", List("account")),
      Parameter("companyName", List("business"))
    )
  )

  private val personalBankAccountExistenceWithName = DataRetrieveDefinition(
    DataRetrieve.Type("personalBankAccountExistenceWithName"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("accountNumberIsWellFormatted"),
          ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountExists"),
          ConstructAttribute.AsIs(Fetch(List("accountExists")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nameMatches"),
          ConstructAttribute.AsIs(Fetch(List("nameMatches")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountName"),
          ConstructAttribute.AsIs(Fetch(List("accountName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
          ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeBankName"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
      )
    ),
    List(
      Parameter("sortCode", List("account")),
      Parameter("accountNumber", List("account")),
      Parameter("name", List("subject"))
    )
  )

  val personalBankAccountExistence = DataRetrieveDefinition(
    DataRetrieve.Type("personalBankAccountExistence"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("accountNumberIsWellFormatted"),
          ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountExists"),
          ConstructAttribute.AsIs(Fetch(List("accountExists")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nameMatches"),
          ConstructAttribute.AsIs(Fetch(List("nameMatches")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("accountName"),
          ConstructAttribute.AsIs(Fetch(List("accountName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
          ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sortCodeBankName"),
          ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
      )
    ),
    List(
      Parameter("sortCode", List("account")),
      Parameter("accountNumber", List("account")),
      Parameter("firstName", List("subject")),
      Parameter("lastName", List("subject"))
    )
  )

  val companyHouseProfile =
    DataRetrieveDefinition(
      DataRetrieve.Type("companyHouseProfile"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("companyName"),
            ConstructAttribute.AsIs(Fetch(List("company_name")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("companyStatus"),
            ConstructAttribute.AsIs(Fetch(List("company_status")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_1"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "address_line_1")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("address_line_2"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "address_line_2")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("po_box"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "po_box")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("locality"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "locality")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("region"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "region")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("postal_code"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "postal_code")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("country"),
            ConstructAttribute.AsIs(Fetch(List("registered_office_address", "country")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("registeredOfficeAddress"),
            ConstructAttribute.Combine(
              List(
                DataRetrieve.Attribute("address_line_1") -> Fetch(List("registered_office_address", "address_line_1")),
                DataRetrieve.Attribute("address_line_2") -> Fetch(List("registered_office_address", "address_line_2")),
                DataRetrieve.Attribute("po_box")         -> Fetch(List("registered_office_address", "po_box")),
                DataRetrieve.Attribute("locality")       -> Fetch(List("registered_office_address", "locality")),
                DataRetrieve.Attribute("region")         -> Fetch(List("registered_office_address", "region")),
                DataRetrieve.Attribute("postal_code")    -> Fetch(List("registered_office_address", "postal_code")),
                DataRetrieve.Attribute("country")        -> Fetch(List("registered_office_address", "country"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("dateOfCreation"),
            ConstructAttribute.AsIs(Fetch(List("date_of_creation")))
          )
        )
      ),
      List(
        Parameter("companyNumber")
      ),
      Map(
        DataRetrieve.Attribute("dateOfCreation") -> DataRetrieve.AttrType.Date
      )
    )

  val companyHouseActiveOfficers =
    DataRetrieveDefinition(
      DataRetrieve.Type("companyHouseActiveOfficers"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("activeDirectors"),
            ConstructAttribute.AsIs(Fetch(List("active_directors")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeSecretaries"),
            ConstructAttribute.AsIs(Fetch(List("active_secretaries")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeLlpMembers"),
            ConstructAttribute.AsIs(Fetch(List("active_llp_members")))
          )
        )
      ),
      List(
        Parameter("companyNumber")
      ),
      Map(
        DataRetrieve.Attribute("activeDirectors")   -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeSecretaries") -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeLlpMembers")  -> DataRetrieve.AttrType.Number
      )
    )

  val ninoInsights = DataRetrieveDefinition(
    DataRetrieve.Type("ninoInsights"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("riskScore"),
          ConstructAttribute.AsIs(Fetch(List("riskScore")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("reason"), ConstructAttribute.AsIs(Fetch(List("reason"))))
      )
    ),
    List(
      Parameter("nino")
    ),
    Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Number)
  )

  val hmrcTaxRates = DataRetrieveDefinition(
    DataRetrieve.Type("hmrcTaxRates"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("regime"),
          ConstructAttribute.AsIs(Fetch(List("regime")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("code"),
          ConstructAttribute.AsIs(Fetch(List("code")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("rate"),
          ConstructAttribute.AsIs(Fetch(List("rate")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("startDate"),
          ConstructAttribute.AsIs(Fetch(List("startDate")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("endDate"),
          ConstructAttribute.AsIs(Fetch(List("endDate")))
        )
      )
    ),
    List(
      Parameter("regime"),
      Parameter("code"),
      Parameter("date", List.empty[String], DataRetrieve.ParamType.Date)
    ),
    Map(
      DataRetrieve.Attribute("regime")    -> DataRetrieve.AttrType.String,
      DataRetrieve.Attribute("code")      -> DataRetrieve.AttrType.String,
      DataRetrieve.Attribute("rate")      -> DataRetrieve.AttrType.Number,
      DataRetrieve.Attribute("startDate") -> DataRetrieve.AttrType.Date,
      DataRetrieve.Attribute("endDate")   -> DataRetrieve.AttrType.Date
    )
  )

  val bankAccountInsights = DataRetrieveDefinition(
    DataRetrieve.Type("bankAccountInsights"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("riskScore"),
          ConstructAttribute.AsIs(Fetch(List("riskScore")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("reason"), ConstructAttribute.AsIs(Fetch(List("reason"))))
      )
    ),
    List(
      Parameter("sortCode"),
      Parameter("accountNumber")
    ),
    Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Number)
  )

  val employments = DataRetrieveDefinition(
    DataRetrieve.Type("employments"),
    Attr.FromArray(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("employerName"),
          ConstructAttribute.AsIs(Fetch(List("employerName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("sequenceNumber"),
          ConstructAttribute.AsIs(Fetch(List("sequenceNumber")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("worksNumber"),
          ConstructAttribute.AsIs(Fetch(List("worksNumber")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("taxDistrictNumber"),
          ConstructAttribute.AsIs(Fetch(List("taxDistrictNumber")))
        ),
        AttributeInstruction(DataRetrieve.Attribute("payeNumber"), ConstructAttribute.AsIs(Fetch(List("payeNumber")))),
        AttributeInstruction(DataRetrieve.Attribute("director"), ConstructAttribute.AsIs(Fetch(List("director"))))
      )
    ),
    List(
      Parameter("nino"),
      Parameter("taxYear", List.empty[String], DataRetrieve.ParamType.Integer)
    ),
    Map(DataRetrieve.Attribute("sequenceNumber") -> DataRetrieve.AttrType.Number)
  )

  val hmrcRosmRegistrationCheck = DataRetrieveDefinition(
    DataRetrieve.Type("hmrcRosmRegistrationCheck"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("postalCode"),
          ConstructAttribute.Concat(
            List(
              Fetch(List("det", "address", "InternationalAddress", "postalCode")),
              Fetch(List("det", "address", "UkAddress", "postalCode"))
            )
          )
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("safeId"),
          ConstructAttribute.AsIs(Fetch(List("det", "safeId")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("organisationName"),
          ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "organisationName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("organisationType"),
          ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "organisationType")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("isAGroup"),
          ConstructAttribute.AsIs(Fetch(List("det", "orgOrInd", "Organisation", "isAGroup")))
        )
      )
    ),
    List(
      Parameter("regime"),
      Parameter("utr")
    )
  )

  val agentDetails = DataRetrieveDefinition(
    DataRetrieve.Type("agentDetails"),
    Attr.FromObject(
      List(
        AttributeInstruction(
          DataRetrieve.Attribute("agencyName"),
          ConstructAttribute.AsIs(Fetch(List("det", "agencyDetails", "agencyName")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("agencyAddress"),
          ConstructAttribute.Concat(
            List(
              Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine1")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine2")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine3")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "addressLine4")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "UkAddress", "postalCode")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine1")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine2")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine3")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "addressLine4")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "postalCode")),
              Fetch(List("det", "agencyDetails", "agencyAddress", "InternationalAddress", "countryCode"))
            )
          )
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("agencyEmail"),
          ConstructAttribute.AsIs(Fetch(List("det", "agencyDetails", "agencyEmail")))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("agencyPhone"),
          ConstructAttribute.AsIs(Fetch(List("det", "contactDetails", "phoneNumber")))
        )
      )
    ),
    List(
      Parameter("agentReferenceNumber")
    )
  )

  private val staticDefinitions = DataRetrieveDefinitions(
    List(
      validateBankDetails,
      businessBankAccountExistence,
      personalBankAccountExistenceWithName,
      personalBankAccountExistence,
      companyHouseProfile,
      companyHouseActiveOfficers,
      ninoInsights,
      bankAccountInsights,
      employments,
      hmrcRosmRegistrationCheck,
      agentDetails,
      hmrcTaxRates
    )
  )

  private def findDefinition(tpe: DataRetrieve.Type): Option[DataRetrieveDefinition] =
    staticDefinitions.definitions.find(_.tpe == tpe)

  def dataRetrieveDateAttrs(): List[String] =
    staticDefinitions.definitions.flatMap {
      _.attrTypeMapping.collect { case (attr, AttrType.Date) =>
        attr.name
      }
    }

  def read(json: JsValue): Opt[DataRetrieve] =
    for {
      typeValue <- DataRetrieve.opt[String](json, "type").map(DataRetrieve.Type(_))
      idValue   <- DataRetrieve.opt[String](json, "id").map(DataRetrieveId(_))
      definition = findDefinition(typeValue).get
      parameters <- DataRetrieve.opt[JsObject](json, "parameters")
      params <-
        definition.parameters.traverse[Opt, DataRetrieve.ParamExpr] { parameter =>
          DataRetrieve
            .opt[String](parameters, parameter.name)
            .flatMap { paramStrExpr =>
              ValueParser.validateWithParser(paramStrExpr, ValueParser.expr)
            }
            .map(expr => ParamExpr(parameter, expr))
        }
      `if` <- DataRetrieve.optOption[IncludeIf](json, "if")
    } yield DataRetrieve(
      tpe = typeValue,
      id = idValue,
      attributes = definition.attributes,
      attrTypeMapping = definition.attrTypeMapping,
      params = params,
      `if` = `if`
    )

}
