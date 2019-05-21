/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate
import play.api.libs.json.{ JsValue, Writes }

package object destinations {
  def createUploadableJson(destination: Destination): String = destination match {
    case hmrcDms: Destination.HmrcDms =>
      import hmrcDms._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("includeIf", destination.includeIf)}
          |  ${optionalField("failOnError", destination.failOnError)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.hmrcDms}",
          |  "dmsFormId": "$dmsFormId",
          |  "customerId": ${TextExpression.format.writes(customerId)},
          |  "classificationType": "$classificationType",
          |  "businessArea": "$businessArea"
          |}""".stripMargin

    case handlebars: Destination.HandlebarsHttpApi =>
      import handlebars._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("payload", payload)}
          |  ${optionalField("convertSingleQuotes", Option(false))}
          |  ${optionalField("includeIf", destination.includeIf)}
          |  ${optionalField("failOnError", destination.failOnError)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.handlebarsHttpApi}",
          |  "profile": ${write(profile)},
          |  "uri": "$uri",
          |  "method": ${write(method)}
          |}""".stripMargin

    case handlebars: Destination.ReviewingOfsted =>
      import handlebars._
      s"""|{
          |  "id": "${id.id}",
          |  "correlationFieldId": "${correlationFieldId.value}",
          |  "reviewFormTemplateId": "${reviewFormTemplateId.value}",
          |  "userId": "${handlebars.userId.value}",
          |  ${optionalField("convertSingleQuotes", Option(false))}
          |  ${optionalField("includeIf", destination.includeIf)}
          |  ${optionalField("failOnError", destination.failOnError)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.reviewingOfsted}"
          |}""".stripMargin

    case handlebars: Destination.ReviewRejection =>
      import handlebars._
      s"""|{
          |  "id": "${id.id}",
          |  "correlationFieldId": "${correlationFieldId.value}",
          |  "reviewFormCommentFieldId": "${reviewFormCommentFieldId.value}",
          |  ${optionalField("convertSingleQuotes", Option(false))}
          |  ${optionalField("includeIf", destination.includeIf)}
          |  ${optionalField("failOnError", destination.failOnError)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.reviewRejection}"
          |}""".stripMargin

    case handlebars: Destination.ReviewApproval =>
      import handlebars._
      s"""|{
          |  "id": "${id.id}",
          |  "correlationFieldId": "${correlationFieldId.value}",
          |  ${optionalField("convertSingleQuotes", Option(false))}
          |  ${optionalField("includeIf", destination.includeIf)}
          |  ${optionalField("failOnError", destination.failOnError)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.reviewApproval}"
          |}""".stripMargin

  }

  def optionalField[T: Writes](fieldName: String, ot: Option[T]): String =
    ot.fold("")(t => s""""$fieldName": ${write(t)},""")

  def write[T](t: T)(implicit w: Writes[T]): JsValue = w.writes(t)
}
