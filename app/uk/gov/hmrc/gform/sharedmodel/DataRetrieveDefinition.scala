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
        )
      )
    ),
    List(
      Parameter("sortCode", List("account")),
      Parameter("accountNumber", List("account")),
      Parameter("companyName", List("business"))
    )
  )

  val personalBankAccountExistenceWithName = DataRetrieveDefinition(
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

  val companyRegistrationNumber = DataRetrieveDefinition(
    DataRetrieve.Type("companyRegistrationNumber"),
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
          DataRetrieve.Attribute("registeredOfficeAddress"),
          ConstructAttribute.Concat(
            List(
              Fetch(List("registered_office_address", "address_line_1")),
              Fetch(List("registered_office_address", "address_line_2")),
              Fetch(List("registered_office_address", "postal_code")),
              Fetch(List("registered_office_address", "locality")),
              Fetch(List("registered_office_address", "region"))
            )
          )
        )
      )
    ),
    List(
      Parameter("companyNumber")
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
    Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Integer)
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
    Map(DataRetrieve.Attribute("riskScore") -> DataRetrieve.AttrType.Integer)
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
    Map(DataRetrieve.Attribute("sequenceNumber") -> DataRetrieve.AttrType.Integer)
  )

  val staticDefinitions = DataRetrieveDefinitions(
    List(
      validateBankDetails,
      businessBankAccountExistence,
      personalBankAccountExistenceWithName,
      personalBankAccountExistence,
      companyRegistrationNumber,
      ninoInsights,
      bankAccountInsights,
      employments
    )
  )

  def findDefinition(tpe: DataRetrieve.Type): Option[DataRetrieveDefinition] =
    staticDefinitions.definitions.find(_.tpe == tpe)

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
