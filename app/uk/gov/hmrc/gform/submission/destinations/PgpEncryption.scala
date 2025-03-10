/*
 * Copyright 2025 HM Revenue & Customs
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

import org.bouncycastle.bcpg.SymmetricKeyAlgorithmTags

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }
import java.security.SecureRandom
import org.bouncycastle.openpgp._
import org.bouncycastle.openpgp.operator.jcajce._
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.Security
import java.util.Date

object PgpEncryption {
  Security.addProvider(new BouncyCastleProvider())

  def createEncryptedData(keyString: String, data: Array[Byte]): Array[Byte] = {
    val encryptionKey = loadPublicKey(keyString)
    val encGen = new PGPEncryptedDataGenerator(
      new JcePGPDataEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256)
        .setWithIntegrityPacket(true)
        .setSecureRandom(new SecureRandom())
        .setProvider(new BouncyCastleProvider())
    )
    encGen.addMethod(
      new JcePublicKeyKeyEncryptionMethodGenerator(encryptionKey).setProvider(new BouncyCastleProvider())
    )

    val encOut = new ByteArrayOutputStream()

    val cOut = encGen.open(encOut, new Array[Byte](1 << 16))

    val lData = new PGPLiteralDataGenerator()
    val pOut = lData.open(cOut, PGPLiteralData.BINARY, PGPLiteralData.CONSOLE, data.length.toLong, new Date())
    pOut.write(data)
    pOut.close()
    cOut.close()
    encOut.toByteArray
  }

  private def loadPublicKey(keyString: String): PGPPublicKey = {
    val keyIn = new ByteArrayInputStream(keyString.getBytes("UTF-8"))
    val pgpPubRing = new PGPPublicKeyRingCollection(PGPUtil.getDecoderStream(keyIn), new JcaKeyFingerprintCalculator())

    val keyIterator = pgpPubRing.getKeyRings
    if (keyIterator.hasNext) {
      keyIterator.next().getPublicKey
    } else {
      throw new IllegalArgumentException("No public key found in the provided key block.")
    }
  }

}
