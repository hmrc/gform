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

package uk.gov.hmrc.gform.cygnum

import java.time.LocalDateTime

object CygnumTemplate {

  val urnTemplate: String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GetData xmlns="http://tempuri.org/">
       |    <Service>GetNewUrn</Service>
       |    <InputParameters>&lt;?xml version=&quot;1.0&quot; encoding=&quot;utf-8&quot;?&gt;&lt;Parameters&gt;&lt;IDs&gt;&lt;ID&gt;SC&lt;/ID&gt;&lt;/IDs&gt;&lt;/Parameters&gt;
       |    </InputParameters>
       |</GetData>
     """.stripMargin

  def formSubmissionTemplate(payload: String): String = {
    val startDateTime = LocalDateTime.now

    s"""<SendData xmlns="http://tempuri.org/">
       |  <Service>SendApplicationForms</Service>
       |  <InputParameters>&lt;?xml version="1.0" encoding="utf-8"?&gt;&lt;Parameters&gt;&lt;DateRange&gt;&lt;StartDateTime&gt;$startDateTime&lt;/StartDateTime&gt;&lt;EndDateTime&gt;${startDateTime
         .plusMinutes(5)}&lt;/EndDateTime&gt;&lt;/DateRange&gt;&lt;/Parameters&gt;</InputParameters>
       |$payload
       |</SendData>
     """.stripMargin
  }
}
