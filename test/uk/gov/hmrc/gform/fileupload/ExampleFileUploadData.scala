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

package uk.gov.hmrc.gform.fileupload

trait ExampleFileUploadData {

  lazy val config = FUConfig(
    fileUploadBaseUrl = "http://fileupload.whatever",
    fileUploadFrontendBaseUrl = "http://fileuploadfrontend.whatever",
    expiryDays = 30,
    maxSize = "20MB",
    maxSizePerItem = "5",
    maxItems = 3
  )
}
