/*
 * Copyright 2020 HM Revenue & Customs
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
import cats.instances.string._
import cats.syntax.show._
import play.api.libs.json.{ JsValue, Writes }

package object destinations {
  def createUploadableJson(destination: Destination): String = destination match {
    case composite: Destination.Composite =>
      import composite._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("includeIf", Option(destination.includeIf), "true")}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.composite}",
          |  "destinations": [
          ${composite.destinations.map(createUploadableJson).toList.mkString(",\n|")}
          |  ]
          |}""".stripMargin

    case stateTransition: Destination.StateTransition =>
      import stateTransition._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("includeIf", Option(destination.includeIf), "true")}
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.stateTransition}",
          |  "requiredState": "${requiredState.toString}"
          |}""".stripMargin

    case hmrcDms: Destination.HmrcDms =>
      import hmrcDms._
      s"""|{
          |  "id": "${id.id}",
          |  ${optionalField("includeIf", Option(destination.includeIf), "true")}
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.hmrcDms}",
          |  "dmsFormId": "$dmsFormId",
          |  "customerId": ${TextExpression.format.writes(TextExpression(customerId))},
          |  "classificationType": "$classificationType",
          |  ${optionalField("roboticsXml", Option(roboticsXml), false)}
          |  ${optionalField("closedStatus", Option(backscan), None)}
          |  "businessArea": "$businessArea"
          |}""".stripMargin

    case submissionConsolidator: Destination.SubmissionConsolidator =>
      s"""|{
          |  "id": "${submissionConsolidator.id.id}",
          |  ${optionalField("includeIf", Option(submissionConsolidator.includeIf), "true")}
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
          |  ${optionalField("convertSingleQuotes", Option(false))}
          |  ${optionalField("includeIf", Option(destination.includeIf), "true")}
          |  ${optionalField("failOnError", Option(destination.failOnError), true)}
          |  ${optionalField("payloadType", Option(handlebars.payloadType), TemplateType.JSON)}
          |  "${Destination.typeDiscriminatorFieldName}": "${Destination.handlebarsHttpApi}",
          |  "profile": ${write(profile)},
          |  "uri": "$uri",
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
             |  ${optionalField("includeIf", Option(destination.includeIf), "true")}
             |  ${optionalField("failOnError", Option(destination.failOnError), true)}
             |  "emailTemplateId": "${email.emailTemplateId}",
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
}
