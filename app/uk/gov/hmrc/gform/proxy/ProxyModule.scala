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

package uk.gov.hmrc.gform.proxy

import java.net.{ Authenticator, InetSocketAddress, PasswordAuthentication, Proxy }
import play.api.Logger
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import uk.gov.hmrc.gform.config.ConfigModule

case class ProxyConfig(
  username: String,
  password: String,
  host: String,
  port: Int,
  protocol: String,
  proxyRequiredForThisEnvironment: Boolean
)

class ProxyModule(val configModule: ConfigModule) {

  implicit def hint: ProductHint[ProxyConfig] = ProductHint(ConfigFieldMapping(CamelCase, CamelCase))

  val maybeProxy: Option[Proxy] =
    ConfigSource.fromConfig(configModule.typesafeConfig.getConfig("proxy")).load[ProxyConfig].toOption.flatMap {
      proxyConfig =>
        if (proxyConfig.proxyRequiredForThisEnvironment) {
          Authenticator
            .setDefault(new ProxyAuthenticator(proxyConfig.username, proxyConfig.password.toCharArray)) // [1] look on javadoc
          val address = new InetSocketAddress(proxyConfig.host, proxyConfig.port)
          Some(new Proxy(Proxy.Type.HTTP, address))
        } else {
          None
        }
    }

  maybeProxy.fold {
    Logger.warn(s"No proxy configuration found. Outbound connections will fail.")
  } { proxy =>
    Logger.info(s"Proxy configuration found. Using proxy: $proxy.")
  }

}

class ProxyAuthenticator(username: String, password: Array[Char]) extends Authenticator {
  Logger.info(s"Proxy password authentication. Username: $username")
  System.setProperty("jdk.http.auth.tunneling.disabledSchemes", "")
  override val getPasswordAuthentication: PasswordAuthentication = new PasswordAuthentication(username, password)
}
