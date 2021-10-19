/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.upscan

import akka.util.ByteString
import cats.instances.string._
import cats.syntax.eq._
import java.awt.image.BufferedImage
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import javax.imageio.{ IIOImage, ImageIO, ImageWriteParam, ImageWriter }
import org.slf4j.{ Logger, LoggerFactory }
import scala.util.control.NonFatal

object ImageCompressor {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def compressIfSupported(input: ByteString, success: UpscanCallback.Success): ByteString =
    if (success.uploadDetails.fileMimeType === "image/jpeg") {
      val compressed = compress(input)
      val from = input.size
      val to = compressed.size
      val ratio = from.toDouble / to
      val spaceSavingPercent = 100.0 - (to * 100.0 / from)
      val spaceSavingBytes = from - to

      val ratioStr = f"$ratio%1.4f"
      val spaceSavingPercentStr = f"$spaceSavingPercent%1.2f"

      logger.info(
        s"Image compressed from $from bytes to $to bytes. Compression ratio $ratioStr, space saving: $spaceSavingBytes bytes ($spaceSavingPercentStr%)"
      )
      compressed

    } else input

  private def compress(input: ByteString): ByteString =
    withResources(new ByteArrayInputStream(input.toArray)) { bais =>
      withResources(new ByteArrayOutputStream()) { baos =>
        withResources(ImageIO.createImageOutputStream(baos)) { imageOutputStream =>
          val imageInputStream = ImageIO.createImageInputStream(bais)
          // ImageIO.read method does close the provided ImageInputStream after the read operation has completed
          val image: BufferedImage = ImageIO.read(imageInputStream)
          val outputImage: IIOImage = new IIOImage(image, null, null)
          val jpegWriter: ImageWriter = ImageIO.getImageWritersBySuffix("jpeg").next()
          val jpegWriteParam: ImageWriteParam = jpegWriter.getDefaultWriteParam()
          jpegWriteParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
          jpegWriteParam.setCompressionQuality(0.5f)
          jpegWriter.setOutput(imageOutputStream)
          jpegWriter.write(null, outputImage, jpegWriteParam)
          jpegWriter.dispose()
          ByteString(baos.toByteArray())
        }
      }
    }

  // https://dkomanov.medium.com/scala-try-with-resources-735baad0fd7d
  def withResources[T <: AutoCloseable, V](r: => T)(f: T => V): V = {
    val resource: T = r
    require(resource != null, "resource is null")
    var exception: Throwable = null
    try f(resource)
    catch {
      case e: Throwable =>
        exception = e
        throw e
    } finally closeAndAddSuppressed(exception, resource)
  }

  private def closeAndAddSuppressed(e: Throwable, resource: AutoCloseable): Unit =
    if (e != null) {
      try resource.close()
      catch {
        case NonFatal(suppressed) =>
          e.addSuppressed(suppressed)
        case fatal: Throwable if NonFatal(e) =>
          fatal.addSuppressed(e)
          throw fatal
        case fatal: InterruptedException =>
          fatal.addSuppressed(e)
          throw fatal
        case fatal: Throwable =>
          e.addSuppressed(fatal)
      }
    } else {
      resource.close()
    }
}
