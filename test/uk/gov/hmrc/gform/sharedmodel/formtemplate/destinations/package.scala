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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.Show
import cats.syntax.show._
import play.api.libs.json.{ JsValue, Writes }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue

package object destinations {
  def createUploadableJson(destination: Destination): String = destination match {
    case composite: Destination.Composite =>
      import composite._
      s"""|{
          |  "id": "${id.id}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.composite}",
          |  "destinations": [
          ${composite.destinations.map(createUploadableJson).toList.mkString(",\n|")}
          |  ]
          |}""".stripMargin

    case stateTransition: Destination.StateTransition =>
      import stateTransition._
      s"""|{
          |  "id": "${id.id}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.stateTransition}",
          |  "requiredState": "${requiredState.toString}"
          |}""".stripMargin

    case hmrcDmsDestination: Destination.HmrcDms =>
      import hmrcDmsDestination._
      s"""|{
          |  "id": "${id.id}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.hmrcDms}",
          |  "dmsFormId": "$dmsFormId",
          |  "customerId": ${TextExpression.format.writes(TextExpression(customerId))},
          |  "classificationType": "$classificationType",
          |  ${optionalField("dataOutputFormat", Option("xml"))}
          |  ${optionalField("closedStatus", Option(backscan), None)}
          |  ${optionalField("formdataXml", Option(formdataXml), false)}
          |  ${optionalField("instructionPdfFields", Option(instructionPdfFields), None)}
          |  "businessArea": "$businessArea"
          |}""".stripMargin

    case dataStoreDestination: Destination.DataStore =>
      import dataStoreDestination._
      s"""|{
          |  "id": "${id.id}",
          |  "routing": "{$destination}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "formId": "${dataStoreDestination.formId}",
          |  "version": "$version",
          |  "taxpayerId": ${TextExpression.format.writes(TextExpression(taxpayerId))},
          |  "regime": "$regime",
          |  "includeSessionInfo": "$includeSessionInfo",
          |  "handlebarPayload": "$handlebarPayload",
          |  "formDataPayload": "$formDataPayload",
          |  ${optionalField("convertSingleQuotes", Option(convertSingleQuotes))}
          |  ${optionalField("payload", Option(payload))}
          |  ${optionalField("validateHandlebarPayload", Option(validateHandlebarPayload))}
          |  ${optionalField("jsonSchema", Option(jsonSchema))}
          |}""".stripMargin

    case infoArchive: Destination.InfoArchive =>
      import infoArchive._
      s"""|{
          |  "id": "${infoArchive.id}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "formId": "${infoArchive.formId}",
          |  "paymentReference": ${TextExpression.format.writes(TextExpression(paymentReference))},
          |  ${optionalField("nino", nino.map(n => TextExpression.format.writes(TextExpression(n))))},
          |  ${optionalField("utr", utr.map(u => TextExpression.format.writes(TextExpression(u))))},
          |  ${optionalField("postalCode", postalCode.map(p => TextExpression.format.writes(TextExpression(p))))}
          |}""".stripMargin

    case submissionConsolidator: Destination.SubmissionConsolidator =>
      s"""|{
          |  "id": "${submissionConsolidator.id.id}",
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(submissionConsolidator.failOnError), true)}
          |  ${optionalField("formData", submissionConsolidator.formData)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.submissionConsolidator}",
          |  "projectId": "${submissionConsolidator.projectId.id}",
          |  "customerId": ${TextExpression.format.writes(TextExpression(submissionConsolidator.customerId))}
          |}""".stripMargin

    case handlebars: Destination.HandlebarsHttpApi =>
      import handlebars._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("payload", payload)}
          |  ${optionalField("convertSingleQuotes", Option(handlebars.convertSingleQuotes))}
          |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  ${optionalField("payloadType", Option(handlebars.payloadType), TemplateType.JSON)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.handlebarsHttpApi}",
          |  "profile": ${write(profile)},
          |  "uri": "$uri",
          |  ${optionalField("multiRequestPayload", Option(handlebars.multiRequestPayload), false)}
          |  "method": ${write(method)}
          |}""".stripMargin

    case log: Destination.Log =>
      import log._
      s"""|{
          |  "id": "${id.id}",
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.log}"
          |}""".stripMargin

    case email: Destination.Email =>
      import email._
      show"""|{
             |  "id": "$id",
             |  ${optionalField("convertSingleQuotes", Option(false))}
             |  "includeIf":  "${getHandlebarValue(destination.includeIf)}",
             |  ${optionalField("failOnError", Option(destination.failOnError), true)}
             |  "emailTemplateId": "${email.emailVerifierService}",
             |  "to": "$to",
             |  "personalisation": {
             |    ${email.personalisation.map { case (k, v) => s"${quote(k)}: ${quote(v)}" }.mkString(", ")}
             |  },
             |  "${Destination.typeDiscriminatorFieldName}": "${Destination.email}"
             |}""".stripMargin
  }

  def optionalField[T: Writes](fieldName: String, ot: Option[T]): String =
    ot.fold("")(t => s""""$fieldName": ${write(t)},""")

  def optionalField[T: Writes](fieldName: String, ot: Option[T], dflt: T): String =
    if (ot.contains(dflt) && Math.random < 0.5) ""
    else optionalField(fieldName: String, ot: Option[T])

  def quote[T](s: T)(implicit show: Show[T]) = show""""$s""""

  def write[T](t: T)(implicit w: Writes[T]): JsValue = w.writes(t)

  private def getHandlebarValue(includeIf: DestinationIncludeIf): String =
    includeIf match {
      case HandlebarValue(s) => s
      case _                 => "true"
    }
}
