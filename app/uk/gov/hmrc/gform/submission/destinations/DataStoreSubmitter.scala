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

package uk.gov.hmrc.gform.submission.destinations

import org.apache.pekko.util.ByteString
import play.api.libs.json.{ JsObject, Json }

import scala.util.Try
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.sdes.SdesRouting
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationId, HandlebarsTemplateProcessorModel, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.sdes.SdesDestination
import uk.gov.hmrc.gform.sharedmodel.{ DataStoreMetaData, LangADT, UserSession }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.DataStore
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.handlebars.{ FocussedHandlebarsModelTree, HandlebarsModelTree, RealHandlebarsTemplateProcessor }
import uk.gov.hmrc.gform.submission.{ DataStoreFileGenerator, RoboticsXMLGenerator }
import org.json4s.native.JsonMethods
import org.json4s.native.Printer.compact
import uk.gov.hmrc.gform.formtemplate.{ HandlebarsSchemaErrorParser, JsonSchemaValidator }
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sdes.datastore.DataStoreWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.http.HeaderCarrier

import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext

class DataStoreSubmitter(
  objectStoreAlgebra: ObjectStoreAlgebra[FOpt],
  dataStoreWorkItemAlgebra: DataStoreWorkItemAlgebra[FOpt]
)(implicit
  ec: ExecutionContext
) extends DataStoreSubmitterAlgebra[FOpt] {

  // Throws an exception, but we cannot recover from it, so it is not reflected in a type as Option[String]
  override def generatePayload(
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    dataStore: DataStore,
    l: LangADT,
    userSession: UserSession,
    taxpayerId: Option[String],
    accumulatedModel: HandlebarsTemplateProcessorModel,
    modelTree: HandlebarsModelTree
  ): String = {
    val dateSubmittedFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.of("Europe/London"))
    val submission = submissionInfo.submission

    val focussedTree = FocussedHandlebarsModelTree(modelTree, modelTree.value.model)
    val maybeHandlebarBasedPayload = dataStore.payload.map(payload =>
      RealHandlebarsTemplateProcessor(payload, accumulatedModel, focussedTree, TemplateType.JSON)
    )

    val rawFormDataBasedPayload = compact(
      JsonMethods.render(
        org.json4s.Xml.toJson(
          RoboticsXMLGenerator.buildDataStoreXML(
            structuredFormData,
            sanitizeRequired = false
          )
        )
      )
    )

    val formDataBasedPayload: JsObject = convertToJson(rawFormDataBasedPayload, dataStore.id, "robotics")
    val handleBarPayload: JsObject = maybeHandlebarBasedPayload.fold(Json.obj()) { handlebarBasedPayload =>
      convertToJson(handlebarBasedPayload, dataStore.id, "handlebar")
    }

    val payloads: JsObject = (dataStore.formDataPayload, dataStore.handlebarPayload) match {
      case (true, true)   => formDataBasedPayload ++ handleBarPayload
      case (true, false)  => formDataBasedPayload
      case (false, true)  => handleBarPayload
      case (false, false) => Json.obj()
    }

    val dataStoreMetaData = DataStoreMetaData(
      dataStore.formId.value,
      dataStore.version,
      "",
      dataStore.regime,
      taxpayerId.getOrElse(""),
      dateSubmittedFormatter.format(submission.submittedDate.toLocalDate),
      submission.submittedDate.toLocalTime.toString,
      submission.submissionRef.value,
      submission.envelopeId.value,
      l.langADTToString.toUpperCase
    )

    DataStoreFileGenerator(userSession, dataStoreMetaData, payloads, dataStore.includeSessionInfo)
  }

  override def validateSchema(
    dataStore: DataStore,
    payload: String
  ): Either[String, Unit] =
    if (dataStore.validateHandlebarPayload) {
      dataStore.jsonSchema match {
        case Some(schema) =>
          JsonSchemaValidator.checkSchema(
            payload,
            schema.toString,
            HandlebarsSchemaErrorParser.parseErrorMessages
          ) match {
            case Left(validationEx) =>
              val errors = Json.prettyPrint(validationEx.errors)
              Left(s"JSON schema validation is failed. JSON validation errors: $errors")
            case Right(value) => Right(value)
          }
        case _ =>
          Left(s"JSON schema does not exist for the destination '${dataStore.id.id}'")
      }
    } else Right(())

  private def convertToJson(string: String, destinationId: DestinationId, payloadDiscriminator: String): JsObject =
    Try(
      Json.parse(string)
    ).fold(
      error =>
        throw new Exception(
          s"Data store destination '${destinationId.id}' error, failed to parse $payloadDiscriminator payload into a json:\n${error.getMessage}",
          error
        ),
      jsValue =>
        jsValue.asOpt[JsObject].getOrElse {
          throw new Exception(
            s"Data store destination '${destinationId.id}' error. Cannot convert $payloadDiscriminator payload into a JsObject. This is not a JsObject: '$jsValue'"
          )
        }
    )

  override def submitPayload(
    submissionInfo: DestinationSubmissionInfo,
    payload: String,
    dataStoreRouting: SdesRouting,
    destination: SdesDestination
  ): FOpt[Unit] = {
    implicit val hc = new HeaderCarrier

    val submission = submissionInfo.submission

    val fileName = s"${submission.envelopeId.value}.json"
    val byteString = ByteString(payload.getBytes)
    val paths = destination.objectStorePaths(submission.envelopeId)
    for {
      _ <- objectStoreAlgebra.uploadFileWithDir(
             paths.permanent,
             fileName,
             byteString,
             ContentType.`application/json`
           )

      objWithSummary <- objectStoreAlgebra.uploadFileWithDir(
                          paths.ephemeral,
                          fileName,
                          byteString,
                          ContentType.`application/json`
                        )
      _ <- dataStoreWorkItemAlgebra.pushWorkItem(
             submission.envelopeId,
             submission.dmsMetaData.formTemplateId,
             submission.submissionRef,
             objWithSummary,
             dataStoreRouting,
             destination
           )
    } yield ()
  }
}
