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

package uk.gov.hmrc.gform.objectstore

import cats.implicits.toFunctorOps
import cats.syntax.eq._
import cats.syntax.traverse._
import org.apache.pekko.NotUsed
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.envelope.EnvelopeAlgebra
import uk.gov.hmrc.gform.objectstore.ObjectStoreService.FileIds._
import uk.gov.hmrc.gform.sdes.SdesConnector
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.envelope.{ EnvelopeData, EnvelopeFile }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.submission.destinations.PgpEncryption
import uk.gov.hmrc.gform.submission.{ PdfAndXmlSummaries, Submission }
import uk.gov.hmrc.gform.time.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client
import uk.gov.hmrc.objectstore.client.{ ObjectSummaryWithMd5, Path }

import java.net.URL
import java.time.format.DateTimeFormatter
import scala.concurrent.{ ExecutionContext, Future }

class ObjectStoreService(
  objectStoreConnector: ObjectStoreConnector,
  envelopeService: EnvelopeAlgebra[Future],
  sdesConnector: SdesConnector
)(implicit
  ec: ExecutionContext
) extends ObjectStoreAlgebra[Future] {
  private val logger = LoggerFactory.getLogger(getClass)

  def createEnvelope(
    formTemplateId: FormTemplateId
  ): Future[EnvelopeId] = {
    logger.info(s"creating envelope, formTemplateId: '${formTemplateId.value}'}")

    val newEnvelope = EnvelopeData.createEnvelope
    for {
      _ <- envelopeService.save(newEnvelope)
    } yield newEnvelope._id
  }

  override def getFileBytes(envelopeId: EnvelopeId, fileName: String, maybeSubDirectory: Option[String])(implicit
    hc: HeaderCarrier
  ): Future[ByteString] =
    objectStoreConnector.getFileBytes(envelopeId, fileName, maybeSubDirectory)

  override def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    fileName: String,
    content: ByteString,
    contentType: ContentType,
    maybeSubDirectory: Option[String]
  )(implicit hc: HeaderCarrier): Future[ObjectSummaryWithMd5] = {
    logger.info(
      s"uploading file: envelopeId - '${envelopeId.value}', fileId - '${fileId.value}', fileName - '$fileName"
    )

    for {
      envelopeData <- getEnvelope(envelopeId)
      _ <- envelopeData.files
             .find(_.fileId === fileId.value)
             .traverse { file =>
               logger.info(s"removing existing file: envelopeId - '$envelopeId', fileName - '${file.fileName}'")
               objectStoreConnector.deleteFile(envelopeId, file.fileName, file.subDirectory)
             }
      res <- objectStoreConnector.uploadFile(
               envelopeId,
               fileName,
               content,
               Some(contentType.value),
               maybeSubDirectory
             )
      _ <- {
        val newFiles =
          envelopeData.files.filterNot(_.fileId === fileId.value) :+
            EnvelopeFile(
              fileId.value,
              fileName,
              FileStatus.Available,
              contentType,
              res.contentLength,
              Map.empty[String, List[String]],
              maybeSubDirectory
            )
        envelopeService.save(envelopeData.copy(files = newFiles))
      }
    } yield res
  }

  override def uploadFileWithDir(
    directory: Path.Directory,
    fileName: String,
    content: ByteString,
    contentType: ContentType
  )(implicit hc: HeaderCarrier): Future[ObjectSummaryWithMd5] =
    objectStoreConnector.uploadFile(
      directory,
      fileName,
      content,
      Some(contentType.value)
    )

  override def getEnvelope(envelopeId: EnvelopeId): Future[EnvelopeData] = envelopeService.get(envelopeId)

  override def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] =
    for {
      envelope <- getEnvelope(envelopeId)
      maybeFileName = envelope.files.find(_.fileId === fileId.value).map(f => (f.fileName, f.subDirectory))
      _ <- maybeFileName match {
             case Some((fileName, maybeSubDirectory)) =>
               logger.info(
                 s"deleting file: envelopeId - '${envelopeId.value}', fileId - '${fileId.value}', fileName - $fileName"
               )
               objectStoreConnector.deleteFile(envelopeId, fileName, maybeSubDirectory)
             case None => Future.failed(new RuntimeException(s"FileId ${fileId.value} not found in mongo"))
           }
      newEnvelope = envelope.copy(files = envelope.files.filterNot(_.fileId === fileId.value))
      _ <- envelopeService.save(newEnvelope)
    } yield ()

  override def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(implicit hc: HeaderCarrier): Future[Unit] = {
    val fileIdsSet = fileIds.map(_.value)
    for {
      envelope <- getEnvelope(envelopeId)
      files = envelope.files.filter(file => fileIdsSet(file.fileId))
      _ <- if (files.size === fileIds.size) {
             files.foreach { file =>
               logger.info(
                 s"deleting file: envelopeId - '${envelopeId.value}', fileId - '${file.fileId}', fileName - ${file.fileName}"
               )
             }
             objectStoreConnector.deleteFiles(envelopeId, files.map(f => (f.fileName, f.subDirectory)))
           } else {
             val foundFileIds = files.map(_.fileId)
             Future.failed(
               new RuntimeException(s"Not all fileIds $fileIds found in mongo. Found fileIds: $foundFileIds")
             )
           }
      newEnvelope = envelope.copy(files = envelope.files.filterNot(file => fileIdsSet(file.fileId)))
      _ <- envelopeService.save(newEnvelope)
    } yield ()
  }

  def deleteFile(directory: Path.Directory, fileName: String)(implicit hc: HeaderCarrier): Future[Unit] =
    objectStoreConnector.deleteFile(directory, fileName)

  def getFile(directory: Path.Directory, fileName: String)(implicit
    hc: HeaderCarrier
  ): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreConnector.getFile(directory, fileName)

  override def zipFiles(
    envelopeId: EnvelopeId,
    objectStorePaths: ObjectStorePaths
  )(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] =
    objectStoreConnector.zipFiles(envelopeId, objectStorePaths)

  override def deleteZipFile(
    envelopeId: EnvelopeId,
    objectStorePaths: ObjectStorePaths
  )(implicit
    hc: HeaderCarrier
  ): Future[Unit] =
    objectStoreConnector.deleteZipFile(envelopeId, objectStorePaths)

  override def getZipFile(
    envelopeId: EnvelopeId,
    objectStorePaths: ObjectStorePaths
  )(implicit hc: HeaderCarrier, m: Materializer): Future[Option[client.Object[Source[ByteString, NotUsed]]]] =
    objectStoreConnector.getZipFile(envelopeId, objectStorePaths)

  override def uploadFromUrl(
    from: URL,
    envelopeId: EnvelopeId,
    fileId: FileId,
    contentType: ContentType,
    fileName: String
  )(implicit
    hc: HeaderCarrier
  ): Future[ObjectSummaryWithMd5] = {
    logger.info(
      s"uploading from url: envelopeId - '${envelopeId.value}', fileId - '${fileId.value}', fileName - '$fileName', contentType - '${contentType.value}'"
    )

    for {
      envelopeData <- getEnvelope(envelopeId)
      _ <- envelopeData.files
             .find(_.fileId === fileId.value)
             .traverse { file =>
               logger.info(s"removing existing file: envelopeId - '$envelopeId', fileName - '${file.fileName}'")
               objectStoreConnector.deleteFile(envelopeId, file.fileName, file.subDirectory)
             }
      res <- objectStoreConnector.uploadFromUrl(from, envelopeId, fileName)
      _ <- {
        val newFiles =
          envelopeData.files.filterNot(_.fileId === fileId.value) :+
            EnvelopeFile(
              fileId.value,
              fileName,
              FileStatus.Available,
              contentType,
              res.contentLength,
              Map.empty[String, List[String]],
              None
            )
        envelopeService.save(envelopeData.copy(files = newFiles))
      }
    } yield res
  }

  override def submitEnvelope(
    submission: Submission,
    summaries: PdfAndXmlSummaries,
    hmrcDms: HmrcDms,
    formTemplateId: FormTemplateId,
    preExistingEnvelope: EnvelopeData
  )(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.debug(s"env-id submit: ${submission.envelopeId}")
    val timeProvider = new TimeProvider
    val date = timeProvider.localDateTime().format(DateTimeFormatter.ofPattern("yyyyMMdd"))
    val fileNamePrefix = s"${submission.submissionRef.withoutHyphens}-$date"

    val fileIdPrefix = hmrcDms.submissionPrefix.fold("")(prefix => s"${prefix}_")

    def duplicateUserFilesF: Future[Unit] =
      hmrcDms.submissionPrefix
        .map { _ =>
          // Duplicate all files at the root level (user uploaded files) into the subdirectory
          preExistingEnvelope.files
            .filter(_.subDirectory.isEmpty)
            .traverse { file =>
              for {
                content <- objectStoreConnector.getFileBytes(submission.envelopeId, file.fileName, None)
                summary <- uploadFile(
                             submission.envelopeId,
                             FileId(file.fileId).prefix(fileIdPrefix),
                             file.fileName,
                             content,
                             file.contentType,
                             hmrcDms.submissionPrefix
                           )
              } yield summary
            }
            .map(_ => ())
        }
        .getOrElse(Future.unit)

    def uploadPfdF: Future[Unit] = {
      val (fileId, fileNameSuffix) = {
        if (hmrcDms.instructionPdfFields.isDefined)
          (customerSummaryPdf.prefix(fileIdPrefix), "customerSummary")
        else (pdf.prefix(fileIdPrefix), "iform")
      }

      uploadFile(
        submission.envelopeId,
        fileId,
        s"$fileNamePrefix-$fileNameSuffix.pdf",
        ByteString(summaries.pdfSummary.pdfContent),
        ContentType.`application/pdf`,
        hmrcDms.submissionPrefix
      ).void
    }

    def uploadInstructionPdfF: Future[Unit] =
      summaries.instructionPdfSummary.fold(Future.successful(())) { iPdf =>
        uploadFile(
          submission.envelopeId,
          pdf.prefix(fileIdPrefix),
          s"$fileNamePrefix-iform.pdf",
          ByteString(iPdf.pdfContent),
          ContentType.`application/pdf`,
          hmrcDms.submissionPrefix
        ).void
      }

    def uploadFormDataF: Future[Unit] =
      summaries.formDataXml
        .map(elem =>
          uploadFile(
            submission.envelopeId,
            formdataXml.prefix(fileIdPrefix),
            s"$fileNamePrefix-formdata.xml",
            ByteString(elem.getBytes),
            ContentType.`application/xml`,
            hmrcDms.submissionPrefix
          ).void
        )
        .getOrElse(Future.unit)

    def roboticsFileExtension = summaries.roboticsFileExtension.map(_.toLowerCase).getOrElse("xml")
    def roboticsFileName = hmrcDms.roboticsFileName(fileNamePrefix, roboticsFileExtension)
    def roboticsAttachmentName = if (
      hmrcDms.roboticsAsAttachment
        .getOrElse(false)
    ) {
      Seq(roboticsFileName)
    } else { Seq() }

    def preExistingEnvelopeNames =
      preExistingEnvelope.files
        .map(_.fileName)
        .filterNot(x => x.endsWith("-iform.pdf") || x.endsWith("-metadata.xml")) ++ roboticsAttachmentName

    def uploadMetadataXmlF: Future[Unit] = {
      val reconciliationId = ReconciliationId.create(submission.submissionRef)
      val metadataXml = MetadataXml.xmlDec + "\n" + MetadataXml
        .getXml(
          submission,
          reconciliationId,
          summaries.instructionPdfSummary.fold(summaries.pdfSummary.numberOfPages)(_.numberOfPages),
          submission.noOfAttachments + summaries.instructionPdfSummary.fold(0)(_ => 1),
          hmrcDms,
          preExistingEnvelopeNames
        )
      uploadFile(
        submission.envelopeId,
        xml.prefix(fileIdPrefix),
        s"$fileNamePrefix-metadata.xml",
        ByteString(metadataXml.getBytes),
        ContentType.`application/xml`,
        hmrcDms.submissionPrefix
      ).void
    }

    def uploadRoboticsContentF: Future[Unit] = summaries.roboticsFile match {
      case Some(elem) =>
        uploadFile(
          submission.envelopeId,
          roboticsFileId(roboticsFileExtension).prefix(fileIdPrefix),
          roboticsFileName,
          ByteString(elem.getBytes),
          getContentType(roboticsFileExtension),
          hmrcDms.submissionPrefix
        ).void
      case _ => Future.unit
    }

    for {
      _ <- duplicateUserFilesF
      _ <- uploadPfdF
      _ <- uploadInstructionPdfF
      _ <- uploadFormDataF
      _ <- uploadRoboticsContentF
      _ <- uploadMetadataXmlF
    } yield ()
  }

  private def getContentType(contentType: String) = contentType match {
    case "json" => ContentType.`application/json`
    case "pdf"  => ContentType.`application/pdf`
    case _      => ContentType.`application/xml`
  }

  override def zipAndEncrypt(envelopeId: EnvelopeId, objectStorePaths: ObjectStorePaths)(implicit
    hc: HeaderCarrier,
    m: Materializer
  ): Future[ObjectSummaryWithMd5] = for {
    _           <- zipFiles(envelopeId, objectStorePaths)
    maybeSource <- getZipFile(envelopeId, objectStorePaths)
    publicKey   <- sdesConnector.getPublicKey()
    objSummary <- maybeSource match {
                    case Some(obj) =>
                      val byteStringFuture: Future[ByteString] =
                        obj.content.runFold(ByteString.empty)(_ ++ _)
                      byteStringFuture.flatMap { concatenatedByteString =>
                        val encrypted =
                          PgpEncryption.createEncryptedData(publicKey.publicKey, concatenatedByteString.toArray)
                        uploadFileWithDir(
                          objectStorePaths.ephemeral,
                          s"${objectStorePaths.zipFilePrefix}${envelopeId.value}.zip",
                          ByteString(encrypted),
                          ContentType.`application/zip`
                        )
                      }
                    case None =>
                      Future.failed(
                        new Exception(
                          s"File ${objectStorePaths.zipFilePrefix}${envelopeId.value}.zip not found in the object store."
                        )
                      )
                  }
  } yield objSummary
}

object ObjectStoreService {

  //forbidden keys. make sure they aren't used in templates
  object FileIds {
    val pdf = FileId("pdf")
    val customerSummaryPdf = FileId("customerSummaryPdf")
    val formdataXml = FileId("formdataXml")
    val xml = FileId("xmlDocument")
    val dataStore = FileId("dataStore")
    def roboticsFileId(extension: String) = FileId(s"robotics${extension.capitalize}")
    val generatedFileIds =
      List(pdf, xml, dataStore, formdataXml, customerSummaryPdf, roboticsFileId("json"), roboticsFileId("xml"))
  }
}
