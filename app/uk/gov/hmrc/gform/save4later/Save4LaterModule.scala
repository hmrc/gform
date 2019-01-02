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

package uk.gov.hmrc.gform.save4later

import uk.gov.hmrc.crypto.{ ApplicationCrypto, CryptoWithKeysFromConfig }
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.wshttp.{ WSHttp, WSHttpModule }
import uk.gov.hmrc.http.cache.client.{ ShortLivedCache, ShortLivedHttpCaching }

class Save4LaterModule(configModule: ConfigModule, wSHttpModule: WSHttpModule) {

  val shortLivedHttpCaching = new ShortLivedHttpCaching {
    override lazy val http: WSHttp = wSHttpModule.auditableWSHttp
    override lazy val defaultSource: String = configModule.appConfig.appName
    override lazy val baseUri: String = configModule.serviceConfig.baseUrl("save4later")
    override lazy val domain: String =
      configModule.serviceConfig
        .getConfString("save4later.domain", throw new Exception(s"Could not find config 'save4later.domain'"))
  }

  val shortLivedCache: ShortLivedCache = new ShortLivedCache {
    override implicit lazy val crypto: CryptoWithKeysFromConfig = ApplicationCrypto.JsonCrypto
    override lazy val shortLiveCache = shortLivedHttpCaching
  }

}
