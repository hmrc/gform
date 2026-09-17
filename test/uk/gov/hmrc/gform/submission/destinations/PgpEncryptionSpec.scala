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

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.security.KeyPairGenerator
import java.util.Date

import org.bouncycastle.bcpg.{ ArmoredOutputStream, PublicKeyAlgorithmTags, PublicKeyPacket }
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.{ PGPEncryptedDataList, PGPLiteralData, PGPObjectFactory, PGPPublicKey, PGPPublicKeyEncryptedData }
import org.bouncycastle.openpgp.operator.jcajce.{ JcaKeyFingerprintCalculator, JcaPGPKeyPair, JcePublicKeyDataDecryptorFactoryBuilder }
import org.bouncycastle.util.io.Streams
import uk.gov.hmrc.gform.Spec

class PgpEncryptionSpec extends Spec {
  "createEncryptedData" should "load the correct public key and encrypt the input" in {
    val input = "test".getBytes(UTF_8)
    val pgpKeyPair = createPgpKeyPair()
    val encryptedData = PgpEncryption.createEncryptedData(armor(pgpKeyPair.getPublicKey), input)

    encryptedData.toSeq should not equal input.toSeq

    val encryptedDataList = new PGPObjectFactory(encryptedData, new JcaKeyFingerprintCalculator())
      .nextObject()
      .asInstanceOf[PGPEncryptedDataList]
    val publicKeyEncryptedData = encryptedDataList.getEncryptedDataObjects
      .next()
      .asInstanceOf[PGPPublicKeyEncryptedData]
    val decryptor = new JcePublicKeyDataDecryptorFactoryBuilder()
      .setProvider(new BouncyCastleProvider())
      .build(pgpKeyPair.getPrivateKey)
    val clearData = publicKeyEncryptedData.getDataStream(decryptor)
    val literalData = new PGPObjectFactory(clearData, new JcaKeyFingerprintCalculator())
      .nextObject()
      .asInstanceOf[PGPLiteralData]
    val decryptedData = Streams.readAll(literalData.getInputStream)

    decryptedData.toSeq shouldBe input.toSeq
    publicKeyEncryptedData.verify() shouldBe true
  }

  private def createPgpKeyPair(): JcaPGPKeyPair = {
    val keyPairGenerator = KeyPairGenerator.getInstance("RSA", new BouncyCastleProvider())
    keyPairGenerator.initialize(2048)
    new JcaPGPKeyPair(
      PublicKeyPacket.VERSION_4,
      PublicKeyAlgorithmTags.RSA_GENERAL,
      keyPairGenerator.generateKeyPair(),
      new Date()
    )
  }

  private def armor(publicKey: PGPPublicKey): String = {
    val output = new ByteArrayOutputStream()
    val armoredOutput = new ArmoredOutputStream(output)
    publicKey.encode(armoredOutput)
    armoredOutput.close()
    output.toString(UTF_8.name())
  }
}
