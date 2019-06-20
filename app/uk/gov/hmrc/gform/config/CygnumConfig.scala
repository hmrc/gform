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

package uk.gov.hmrc.gform.config

import java.util.Base64
import pureconfig.generic.auto._

trait CygnumConfig {
  val cygnumSoap: CygnumSoapApiConf =
    pureconfig.loadConfigOrThrow[CygnumSoapApiConf]("microservice.destination-services.cygnum.cygnum-conf")

  import cygnumSoap._

  lazy val cygnumURL: String = client.url
  lazy val cygnumClientPassword: String = new String(Base64.getDecoder.decode(client.base64KeystorePassword))
  lazy val cygnumKeyStore: String = client.base64Keystore
  lazy val cygnumPrivateKeyAlias: String = new String(Base64.getDecoder.decode(client.base64PrivateKeyAlias))
  lazy val cygnumUsername: String = new String(Base64.getDecoder.decode(userAuth.base64Username))
  lazy val cygnumPassword: String = new String(Base64.getDecoder.decode(userAuth.base64Password))
}

case class CygnumSoapApiConf(userAuth: CygnumUserAuth, client: CygnumClient)
case class CygnumUserAuth(base64Username: String, base64Password: String)
case class CygnumClient(
  url: String,
  base64KeystoreType: String = "jks",
  base64Keystore: String,
  base64KeystorePassword: String,
  base64PrivateKeyAlias: String)
