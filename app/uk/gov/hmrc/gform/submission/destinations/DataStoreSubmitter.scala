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

import akka.util.ByteString
import uk.gov.hmrc.gform.core.FOpt
import uk.gov.hmrc.gform.sharedmodel.{ DataStoreMetaData, LangADT, UserSession }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.DataStore
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue
import uk.gov.hmrc.gform.submission.{ DataStoreFileGenerator, RoboticsXMLGenerator }
import org.json4s.native.JsonMethods
import org.json4s.native.Printer.compact
import uk.gov.hmrc.gform.objectstore.ObjectStoreAlgebra
import uk.gov.hmrc.gform.sdes.datastore.DataStoreWorkItemAlgebra
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.Path

import java.time.{ Instant, ZoneId }
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext

class DataStoreSubmitter(
  objectStoreAlgebra: ObjectStoreAlgebra[FOpt],
  dataStoreWorkItemAlgebra: DataStoreWorkItemAlgebra[FOpt],
  timeProvider: TimeProvider,
  dataStorebasePath: String,
  sdesBasePath: String
)(implicit
  ec: ExecutionContext
) extends DataStoreSubmitterAlgebra[FOpt] {
  override def apply(
    submissionInfo: DestinationSubmissionInfo,
    structuredFormData: StructuredFormValue.ObjectStructure,
    dataStore: DataStore,
    l: LangADT,
    userSession: UserSession,
    taxpayerId: Option[String]
  ): FOpt[Unit] = {
    implicit val hc = new HeaderCarrier
    val dateSubmittedFormater = DateTimeFormatter.ofPattern("dd/MM/yyyy").withZone(ZoneId.of("Europe/London"))
    val submission = submissionInfo.submission

    val gform: String =
      compact(
        JsonMethods.render(
          org.json4s.Xml.toJson(
            RoboticsXMLGenerator(
              submission.dmsMetaData.formTemplateId,
              dataStore.formId.value,
              submission.submissionRef,
              structuredFormData,
              Instant.now(),
              l,
              Some(submission.envelopeId),
              sanitizeRequired = false
            )
          )
        )
      )

    val dataStoreMetaData = DataStoreMetaData(
      dataStore.formId.value,
      dataStore.version.version,
      "",
      dataStore.regime,
      taxpayerId.getOrElse(""),
      dateSubmittedFormater.format(submission.submittedDate.toLocalDate),
      submission.submittedDate.toLocalTime.toString
    )

    val dataStoreJson = DataStoreFileGenerator(userSession, dataStoreMetaData, gform, dataStore.includeSessionInfo)
    val fileName = s"${submission.envelopeId.value}.json"
    for {
      _ <- objectStoreAlgebra.uploadFile(
             Path.Directory(s"${dataStorebasePath}envelopes/${submission.envelopeId.value}"),
             fileName,
             ByteString(dataStoreJson.getBytes),
             ContentType.`application/json`
           )

      objWithSummary <- objectStoreAlgebra.uploadFile(
                          Path.Directory(s"$sdesBasePath$dataStorebasePath"),
                          fileName,
                          ByteString(dataStoreJson.getBytes),
                          ContentType.`application/json`
                        )
      _ <- dataStoreWorkItemAlgebra.pushWorkItem(
             submission.envelopeId,
             submission.dmsMetaData.formTemplateId,
             submission.submissionRef,
             objWithSummary
           )
    } yield ()
  }
}
