/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileUpload.FileUploadModule
import uk.gov.hmrc.gform.theauditing.AuditingModule
import uk.gov.hmrc.gform.time.TimeModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class TheApplicationModule {
  val configModule = new ConfigModule
  val auditingModule = new AuditingModule(configModule)
  val wSHttpModule = new WSHttpModule(auditingModule, configModule)
  val timeModule = new TimeModule
  val fileUploadModule = new FileUploadModule(configModule, wSHttpModule, timeModule)
}
